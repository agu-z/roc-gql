interface Gql.Output
    exposes [
        TypeMeta,
        Type,
        ResolveErr,
        FieldMeta,
        ObjectMeta,
        EnumMeta,
        string,
        int,
        boolean,
        listOf,
        nullable,
        ref,
        object,
        field,
        resolveObject,
    ] imports [
        Gql.Document.{ Selection },
        Gql.Value.{ Value },
        Gql.Input.{ Input, Argument },
    ]

Type a : {
    type : TypeMeta,
    resolve : a, List Selection, Dict Str Value -> Result Value ResolveErr,
}

TypeMeta : [
    String,
    Int,
    Boolean,
    List TypeMeta,
    Ref
        # Can't use aliases because compiler stack overflows
        {
            name : Str,
            fields : List {
                name : Str,
                type : TypeMeta,
                arguments : List Argument,
            },
        },
    Nullable TypeMeta,
    Enum EnumMeta,
]

EnumMeta : {
    name : Str,
    values : List EnumValue,
}

EnumValue : {
    name : Str,
}

ResolveErr : [
    FieldNotFound Str Str,
    InputErr Gql.Input.Error,
    VarNotFound Str,
    InvalidEnumValue,
]

Object a : {
    meta : ObjectMeta,
    resolvers : Dict Str (FieldResolve a),
}

ObjectMeta : {
    name : Str,
    fields : List FieldMeta,
}

Field a : {
    meta : FieldMeta,
    resolve : FieldResolve a,
}

FieldMeta : {
    name : Str,
    type : TypeMeta,
    arguments : List Argument,
}

FieldResolve a : a, Dict Str Value, List Selection, Dict Str Value -> Result Value ResolveErr

string : Type Str
string = {
    type: String,
    resolve: \a, _, _ -> Ok (String a),
}

int : Type I32
int = {
    type: Int,
    resolve: \a, _, _ -> Ok (Int a),
}

boolean : Type Bool
boolean = {
    type: Boolean,
    resolve: \a, _, _ -> Ok (Boolean a),
}

listOf : Type a -> Type (List a)
listOf = \itemType -> {
    type: List itemType.type,
    resolve: \list, selection, vars ->
        list
        |> List.mapTry \item -> itemType.resolve item selection vars
        |> Result.map List,
}

nullable : Type a -> Type (Result a [Nothing])
nullable = \itemType -> {
    type: Nullable itemType.type,
    resolve: \value, selection, vars ->
        value
        |> Result.map \item -> itemType.resolve item selection vars
        |> Result.withDefault (Ok Null),
}

ref : Object a -> Type a
ref = \obj -> {
    type: Ref obj.meta,
    resolve: \value, selection, vars -> resolveObject obj value selection vars,
}

object : Str, List (Field a) -> Object a
object = \name, fields -> {
    meta: {
        name,
        fields: fields |> List.map .meta,
    },
    resolvers: fields
    |> List.map \f -> (f.meta.name, f.resolve)
    |> Dict.fromList,
}

field :
    Str,
    Type output,
    {
        takes : Input input,
        resolve : a, input -> output,
    }
    -> Field a
field = \name, returns, { takes, resolve } -> {
    meta: {
        name,
        type: returns.type,
        arguments: Gql.Input.arguments takes,
    },
    resolve: \obj, args, selection, vars ->
        args
        |> Gql.Input.decode takes
        |> Result.mapErr InputErr
        |> Result.try \input ->
            returns.resolve (resolve obj input) selection vars,
}

resolveObject : Object a, a, List Selection, Dict Str Value -> Result Value ResolveErr
resolveObject = \obj, a, selectionSet, vars ->
    selectionSet
    |> List.mapTry \selection ->
        when selection is
            Field opField ->
                outName =
                    opField.alias
                    |> Result.withDefault opField.field

                if opField.field == "__typename" then
                    Ok (outName, String obj.meta.name)
                else
                    fieldResolver <-
                        obj.resolvers
                        |> Dict.get opField.field
                        |> Result.mapErr \KeyNotFound -> FieldNotFound obj.meta.name opField.field
                        |> Result.try

                    argsDict <-
                        opField.arguments
                        |> List.mapTry \(key, docValue) ->
                            docValue
                            |> Gql.Value.fromDocument vars
                            |> Result.map \value -> (key, value)
                        |> Result.map Dict.fromList
                        |> Result.try

                    value <- fieldResolver a argsDict opField.selectionSet vars
                        |> Result.map

                    (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

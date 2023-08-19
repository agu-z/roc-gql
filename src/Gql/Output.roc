interface Gql.Output
    exposes [
        TypeName,
        Type,
        ResolveErr,
        string,
        int,
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
    type : TypeName,
    resolve : a, List Selection, Dict Str Value -> Result Value ResolveErr,
}

TypeName : [
    String,
    Int,
    List TypeName,
    Ref Str,
    Nullable TypeName,
    Enum Str (List EnumValue),
]

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
    name : Str,
    fields : Dict Str (Field a),
}

Field a : {
    name : Str,
    type : TypeName,
    arguments : List Argument,
    resolve : a, Dict Str Value, List Selection, Dict Str Value -> Result Value ResolveErr,
}

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
    type: Ref obj.name,
    resolve: \value, selection, vars -> resolveObject obj value selection vars,
}

object : Str, List (Field a) -> Object a
object = \name, fields -> {
    name,
    fields: fields
    |> List.map \f -> (f.name, f)
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
    name,
    type: returns.type,
    arguments: Gql.Input.arguments takes,
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
                schemaField <-
                    obj.fields
                    |> Dict.get opField.field
                    |> Result.mapErr \KeyNotFound -> FieldNotFound obj.name opField.field
                    |> Result.try

                outName =
                    opField.alias
                    |> Result.withDefault opField.field

                argsDict <-
                    opField.arguments
                    |> List.mapTry \(key, docValue) ->
                        docValue
                        |> Gql.Value.fromDocument vars
                        |> Result.map \value -> (key, value)
                    |> Result.map Dict.fromList
                    |> Result.try

                value <- schemaField.resolve a argsDict opField.selectionSet vars
                    |> Result.map

                (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

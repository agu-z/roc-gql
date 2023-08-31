interface Gql.Output
    exposes [
        TypeMeta,
        Type,
        ResolveErr,
        FieldMeta,
        ObjectMeta,
        EnumMeta,
        OpContext,
        string,
        int,
        boolean,
        listOf,
        nullable,
        ref,
        object,
        field,
        addField,
        resolveObject,
        describe,
    ] imports [
        Gql.Document.{ CanSelection, Document },
        Gql.Value.{ Value },
        Gql.Input.{ Input, Argument },
    ]

Type a : {
    type : TypeMeta,
    resolve : a, List CanSelection, OpContext -> Result Value ResolveErr,
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
                description : Result Str [Nothing],
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

Field a := {
    meta : FieldMeta,
    resolve : FieldResolve a,
}
    implements [
        Documentable {
            describe: describeField,
        },
    ]

describeField = \@Field f, description ->
    fieldMeta = f.meta
    @Field { f & meta: { fieldMeta & description: Ok description } }

FieldMeta : {
    name : Str,
    description : Result Str [Nothing],
    type : TypeMeta,
    arguments : List Argument,
}

FieldResolve a : a, Dict Str Value, List CanSelection, OpContext -> Result Value ResolveErr

OpContext : {
    variables : Dict Str Value,
    document : Document,
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
    resolve: \value, selection, opCtx -> resolveObject obj value selection opCtx,
}

object : Str, List (Field a) -> Object a
object = \name, fields ->
    len = List.len fields

    default = {
        meta: { name, fields: List.withCapacity len },
        resolvers: Dict.withCapacity len,
    }

    List.walk fields default addField

addField : Object a, Field a -> Object a
addField = \obj, @Field f -> {
    meta: {
        name: obj.meta.name,
        fields: obj.meta.fields
        |> List.append f.meta,
    },
    resolvers: obj.resolvers
    |> Dict.insert f.meta.name f.resolve,
}

field :
    Str,
    Type output,
    {
        takes : Input input,
        resolve : a, input -> output,
    }
    -> Field a
field = \name, returns, { takes, resolve } -> @Field {
        meta: {
            name,
            description: Err Nothing,
            type: returns.type,
            arguments: Gql.Input.arguments takes,
        },
        resolve: \obj, args, selection, opCtx ->
            args
            |> Gql.Input.decode takes
            |> Result.mapErr InputErr
            |> Result.try \input ->
                returns.resolve (resolve obj input) selection opCtx,
    }

resolveObject : Object a, a, List CanSelection, OpContext -> Result Value ResolveErr
resolveObject = \obj, a, selectionSet, opCtx ->
    selectionSet
    |> List.mapTry \CanField opField ->
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
                    |> Gql.Value.fromDocument opCtx.variables
                    |> Result.map \value -> (key, value)
                |> Result.map Dict.fromList
                |> Result.try

            value <- fieldResolver a argsDict opField.selectionSet opCtx
                |> Result.map

            (outName, value)
    |> Result.map Object

# Docs

Documentable implements
    describe : v, Str -> v where v implements Documentable

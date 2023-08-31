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
        objectMeta,
    ] imports [
        Gql.Document.{ CanSelection, Document },
        Gql.Docs.{ Describe, Deprecate },
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
            description : Result Str [Nothing],
            fields : List {
                name : Str,
                type : TypeMeta,
                arguments : List Argument,
                description : Result Str [Nothing],
                deprecationReason : Result Str [Nothing],
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

Object a := {
    meta : ObjectMeta,
    resolvers : Dict Str (FieldResolve a),
}
    implements [
        Describe {
            describe: describeObject,
        },
        # GraphQL spec doesn't allow objects to be deprecated
    ]

describeObject : Object a, Str -> Object a
describeObject = \@Object obj, description ->
    objMeta = obj.meta
    @Object { obj & meta: { objMeta & description: Ok description } }

ObjectMeta : {
    name : Str,
    description : Result Str [Nothing],
    fields : List FieldMeta,
}

Field a := {
    meta : FieldMeta,
    resolve : FieldResolve a,
}
    implements [
        Describe {
            describe: describeField,
        },
        Deprecate {
            deprecate: deprecateField,
        },
    ]

describeField : Field a, Str -> Field a
describeField = \@Field f, description ->
    fieldMeta = f.meta
    @Field { f & meta: { fieldMeta & description: Ok description } }

deprecateField : Field a, Str -> Field a
deprecateField = \@Field f, reason ->
    fieldMeta = f.meta
    @Field { f & meta: { fieldMeta & deprecationReason: Ok reason } }

FieldMeta : {
    name : Str,
    type : TypeMeta,
    arguments : List Argument,
    description : Result Str [Nothing],
    deprecationReason : Result Str [Nothing],
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
ref = \@Object obj -> {
    type: Ref obj.meta,
    resolve: \value, selection, opCtx -> resolveObject (@Object obj) value selection opCtx,
}

object : Str, List (Field a) -> Object a
object = \name, fields ->
    len = List.len fields

    default = @Object {
        meta: {
            name,
            description: Err Nothing,
            fields: List.withCapacity len,
        },
        resolvers: Dict.withCapacity len,
    }

    List.walk fields default addField

addField : Object a, Field a -> Object a
addField = \@Object obj, @Field f ->
    @Object {
        meta: {
            name: obj.meta.name,
            description: obj.meta.description,
            fields: obj.meta.fields
            |> List.append f.meta,
        },
        resolvers: obj.resolvers
        |> Dict.insert f.meta.name f.resolve,
    }

objectMeta : Object * -> ObjectMeta
objectMeta = \@Object obj -> obj.meta

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
            type: returns.type,
            arguments: Gql.Input.arguments takes,
            description: Err Nothing,
            deprecationReason: Err Nothing,
        },
        resolve: \obj, args, selection, opCtx ->
            args
            |> Gql.Input.decode takes
            |> Result.mapErr InputErr
            |> Result.try \input ->
                returns.resolve (resolve obj input) selection opCtx,
    }

resolveObject : Object a, a, List CanSelection, OpContext -> Result Value ResolveErr
resolveObject = \@Object obj, a, selectionSet, opCtx ->
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


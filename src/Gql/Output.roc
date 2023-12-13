interface Gql.Output
    exposes [
        TypeMeta,
        Type,
        ResolveErr,
        FieldMeta,
        ObjectMeta,
        EnumMeta,
        EnumCaseMeta,
        OpContext,
        string,
        int,
        boolean,
        listOf,
        nullable,
        ref,
        object,
        field,
        retField,
        addField,
        mapField,
        resolveObject,
        objectMeta,
        resolveErrToStr,
    ] imports [
        Gql.Document.{ CanSelection, Document },
        Gql.Docs.{ Describe, Deprecate },
        Gql.Value.{ Value },
        Gql.Input.{ Input, Argument },
    ]

Type a out : {
    type : TypeMeta,
    resolve : a, List CanSelection, OpContext -> Result out ResolveErr,
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
    description : Result Str [Nothing],
    cases : List EnumCaseMeta,
}

EnumCaseMeta : {
    name : Str,
    description : Result Str [Nothing],
    deprecationReason : Result Str [Nothing],
}

ResolveErr : [
    FieldNotFound Str Str,
    InputErr Str Gql.Input.Error,
    VarNotFound Str,
]

resolveErrToStr : ResolveErr -> Str
resolveErrToStr = \err ->
    when err is
        FieldNotFound objName fieldName ->
            "Field `\(fieldName)` not found on `\(objName)`"

        InputErr fieldName inputErr ->
            # TODO: Show object name
            "On `\(fieldName)`: \(Gql.Input.errToStr inputErr)"

        VarNotFound varName ->
            "Variable \(varName) not found"

Object a out := {
    meta : ObjectMeta,
    resolvers : Dict Str (FieldResolve a out),
}
    implements [
        Describe {
            describe: describeObject,
        },
        # GraphQL spec doesn't allow objects to be deprecated
    ]

describeObject : Object a out, Str -> Object a out
describeObject = \@Object obj, description ->
    objMeta = obj.meta
    @Object { obj & meta: { objMeta & description: Ok description } }

ObjectMeta : {
    name : Str,
    description : Result Str [Nothing],
    fields : List FieldMeta,
}

Field a out := {
    meta : FieldMeta,
    resolve : FieldResolve a out,
}
    implements [
        Describe {
            describe: describeField,
        },
        Deprecate {
            deprecate: deprecateField,
        },
    ]

describeField : Field a out, Str -> Field a out
describeField = \@Field f, description ->
    fieldMeta = f.meta
    @Field { f & meta: { fieldMeta & description: Ok description } }

deprecateField : Field a out, Str -> Field a out
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

FieldResolve a out : a, Dict Str Value, List CanSelection, OpContext -> Result out ResolveErr

OpContext : {
    variables : Dict Str Value,
    document : Document,
}

string : Type Str Value
string = {
    type: String,
    resolve: \a, _, _ -> Ok (String a),
}

int : Type I32 Value
int = {
    type: Int,
    resolve: \a, _, _ -> Ok (Int a),
}

boolean : Type Bool Value
boolean = {
    type: Boolean,
    resolve: \a, _, _ -> Ok (Boolean a),
}

listOf : Type a Value -> Type (List a) Value
listOf = \itemType -> {
    type: List itemType.type,
    resolve: \list, selection, vars ->
        list
        |> List.mapTry \item -> itemType.resolve item selection vars
        |> Result.map List,
}

nullable : Type a Value -> Type (Result a [Nothing]) Value
nullable = \itemType -> {
    type: Nullable itemType.type,
    resolve: \value, selection, opCtx ->
        value
        |> Result.map \item -> itemType.resolve item selection opCtx
        |> Result.withDefault (Ok Null),
}

ref : Object a Value -> Type a Value
ref = \@Object obj -> {
    type: Ref obj.meta,
    resolve: \value, selection, opCtx ->
        resolveObject (@Object obj) String value selection opCtx
        |> Result.map Object,
}

object : Str, List (Field a out) -> Object a out
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

addField : Object a out, Field a out -> Object a out
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

objectMeta : Object * * -> ObjectMeta
objectMeta = \@Object obj -> obj.meta

field :
    Str,
    Type result out,
    {
        takes : Input input Gql.Input.Anonymous,
        resolve : a, input -> result,
    }
    -> Field a out
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
            |> Result.mapErr \err -> InputErr name err
            |> Result.try \input ->
                returns.resolve (resolve obj input) selection opCtx,
    }

retField : Str, Type result out, (a -> result) -> Field a out
retField = \name, returns, resolve ->
    field name returns { takes: Gql.Input.none, resolve: \a, _ -> resolve a }

mapField : Field obj a, (a -> b) -> Field obj b
mapField = \@Field f, toB ->
    @Field {
        meta: f.meta,
        resolve: \obj, args, selection, opCtx ->
            f.resolve obj args selection opCtx
            |> Result.map toB,
    }

resolveObject : Object a out, (Str -> out), a, List CanSelection, OpContext -> Result (List (Str, out)) ResolveErr
resolveObject = \@Object obj, strToOut, a, selectionSet, opCtx ->
    selectionSet
    |> List.mapTry \CanField opField ->
        outName =
            opField.alias
            |> Result.withDefault opField.field

        if opField.field == "__typename" then
            Ok (outName, strToOut obj.meta.name)
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


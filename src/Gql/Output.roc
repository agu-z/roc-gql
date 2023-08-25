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

Type a schema : schema -> {
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

Object a schema : {
    name : Str,
    fields : Dict Str (Field a schema),
}

Field a schema : {
    name : Str,
    type : schema -> TypeName,
    arguments : List Argument,
    resolve : a, Dict Str Value, schema, List Selection, Dict Str Value -> Result Value ResolveErr,
}

string : Type Str *
string = \_ -> {
    type: String,
    resolve: \a, _, _ -> Ok (String a),
}

int : Type I32 *
int = \_ -> {
    type: Int,
    resolve: \a, _, _ -> Ok (Int a),
}

listOf : Type a schema -> Type (List a) schema
listOf = \toItemType -> \schema -> 
    itemType = toItemType schema

    {
        type: List itemType.type,
        resolve: \list, selection, vars ->
            list
            |> List.mapTry \item -> itemType.resolve item selection vars
            |> Result.map List,
    }

nullable : Type a schema -> Type (Result a [Nothing]) schema
nullable = \toItemType -> \schema -> 
    itemType = toItemType schema

    {
        type: Nullable itemType.type,
        resolve: \value, selection, vars ->
            value
            |> Result.map \item -> itemType.resolve item selection vars
            |> Result.withDefault (Ok Null),
    }

ref : (schema -> Object a schema) -> Type a schema
ref = \toObj -> \schema -> 
    obj = toObj schema
    {
        type: Ref obj.name,
        resolve: \value, selection, vars -> resolveObject obj value schema selection vars,
    }

object : Str, List (Field a schema) -> Object a schema
object = \name, fields -> {
    name,
    fields: 
        fields
        |> List.map \f -> (f.name, f)
        |> Dict.fromList,
}

field :
    Str,
    Type output schema,
    {
        takes : Input input,
        resolve : a, input -> output,
    }
    -> Field a schema
field = \name, toReturn, { takes, resolve } -> {
    name,
    type: \schema -> (toReturn schema).type,
    arguments: Gql.Input.arguments takes,
    resolve: \obj, args, schema, selection, vars ->
        args
        |> Gql.Input.decode takes
        |> Result.mapErr InputErr
        |> Result.try \input ->
            (toReturn schema).resolve (resolve obj input) selection vars,
}

resolveObject : Object a schema, a, schema, List Selection, Dict Str Value -> Result Value ResolveErr
resolveObject = \obj, a, schema, selectionSet, vars ->
    selectionSet
    |> List.mapTry \selection ->
        when selection is
            Field opField ->
                outName =
                    opField.alias
                    |> Result.withDefault opField.field

                if opField.field == "__typename" then
                    Ok (outName, String obj.name)
                else
                    schemaField <-
                        obj.fields 
                        |> Dict.get opField.field
                        |> Result.mapErr \KeyNotFound -> FieldNotFound obj.name opField.field
                        |> Result.try

                    argsDict <-
                        opField.arguments
                        |> List.mapTry \(key, docValue) ->
                            docValue
                            |> Gql.Value.fromDocument vars
                            |> Result.map \value -> (key, value)
                        |> Result.map Dict.fromList
                        |> Result.try

                    value <- schemaField.resolve a argsDict schema opField.selectionSet vars
                        |> Result.map

                    (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

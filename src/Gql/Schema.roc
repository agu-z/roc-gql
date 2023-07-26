interface Gql.Schema
    exposes []
    imports [
        Gql.Document.{ Document },
        Gql.Parse.{ parseDocument },
    ]

Schema : {
    query : Type,
}

schema : { query : Type } -> Schema
schema = \{ query } -> { query }

Type : {
    name : Str,
    fields : Dict Str Field,
}

type : Str -> Type
type = \name ->
    { name, fields: Dict.withCapacity 5 }

Field : {
    type : [String],
    resolve : {} -> Value,
}

Value : [
    String Str,
    Object (List (Str, Value)),
]

field = \obj, name, typeRef, resolve ->
    newField = {
        type: typeRef.type,
        resolve: \input -> typeRef.encode (resolve input),
    }

    { obj & fields: obj.fields |> Dict.insert name newField }

string = {
    type: String,
    encode: String,
}

execute :
    {
        schema : Schema,
        document : Document,
        operation : [First, ByName Str],
    }
    -> Result _ _
execute = \params ->
    operation <-
        params.document
        |> Gql.Document.findOperation params.operation
        |> Result.try

    obj = params.schema.query

    operation.selectionSet
    |> List.mapTry \selection ->
        when selection is
            Field opField ->
                schemaField <-
                    obj.fields
                    |> Dict.get opField.field
                    |> Result.mapErr \KeyNotFound -> FieldNotFound obj.name opField.field
                    |> Result.map

                outName =
                    opField.alias
                    |> Result.withDefault opField.field

                value = schemaField.resolve {}

                (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

expect
    query =
        type "Query"
        |> field "userName" string \_ -> "agu-z"

    mySchema = schema { query }

    result =
        document <-
            parseDocument "query GetUserName { userName }"
            |> Result.try

        execute {
            schema: mySchema,
            document,
            operation: ByName "GetUserName",
        }

    result == Ok (Object [("userName", String "agu-z")])


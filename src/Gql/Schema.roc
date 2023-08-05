interface Gql.Schema
    exposes [
        Schema,
        Type,
        Field,
        TypeName,
        Object,
        schema,
        object,
        field,
        int,
        string,
        execute,
    ]
    imports [
        Gql.Document.{ Document },
        Gql.Parse.{ parseDocument },
        Gql.Schema.Input.{ Input, Argument, const, optional },
        Gql.Value.{ Value },
    ]

Schema : {
    query : Object,
}

schema : { query : Object } -> Schema
schema = \{ query } -> { query }

Object : {
    name : Str,
    fields : Dict Str Field,
}

object : Str -> Object
object = \name ->
    { name, fields: Dict.withCapacity 5 }

Field : {
    type : TypeName,
    arguments : List Argument,
    resolve : Dict Str Value -> Result Value Gql.Schema.Input.Error,
}

TypeName : [
    String,
    Int,
]

Type a : {
    type : TypeName,
    encode : a -> Value,
}

field : Object,
    Str,
    Type output,
    {
        takes : Input input,
        resolve : input -> output,
    }
    -> Object
field = \obj, name, returns, { takes, resolve } ->
    newField = {
        type: returns.type,
        arguments: Gql.Schema.Input.arguments takes,
        resolve: \inputValue ->
            inputValue
            |> Gql.Schema.Input.decode takes
            |> Result.map \input ->
                input
                |> resolve
                |> returns.encode,
    }

    { obj & fields: obj.fields |> Dict.insert name newField }

string : Type Str
string = {
    type: String,
    encode: String,
}

int : Type I32
int = {
    type: Int,
    encode: Int,
}

execute :
    {
        schema : Schema,
        document : Document,
        operation : [First, ByName Str],
        variables : Dict Str Value,
    }
    -> Result
        Value
        [
            FieldNotFound Str Str,
            InputErr Gql.Schema.Input.Error,
            OperationNotFound,
            VarNotFound Str,
        ]
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
                    |> Result.try

                outName =
                    opField.alias
                    |> Result.withDefault opField.field

                argsDict <-
                    opField.arguments
                    |> List.mapTry \(key, docValue) ->
                        docValue
                        |> Gql.Value.fromDocument params.variables
                        |> Result.map \value -> (key, value)
                    |> Result.map Dict.fromList
                    |> Result.try

                value <- schemaField.resolve argsDict
                    |> Result.mapErr InputErr
                    |> Result.map

                (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

expect
    query : Object
    query =
        object "Query"
        |> field "greet" string {
            takes: const {
                name: <- optional "name" Gql.Schema.Input.string,
            },
            resolve: \{ name } ->
                "Hi, \(Result.withDefault name "friend")!",
        }

    mySchema = schema { query }

    result =
        document <-
            parseDocument
                """
                query {
                    greet
                    hiMatt: greet(name: "Matt")
                }
                """
            |> Result.try

        execute {
            schema: mySchema,
            document,
            operation: First,
            variables: Dict.empty {},
        }

    result
    == Ok
        (
            Object [
                ("greet", String "Hi, friend!"),
                ("hiMatt", String "Hi, Matt!"),
            ]
        )


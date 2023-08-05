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
        Gql.Input.{ Input, Argument, const, optional, required },
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

object : Str, List Field -> Object
object = \name, fields -> {
    name,
    fields: fields
    |> List.map \f -> (f.name, f)
    |> Dict.fromList,
}

Field : {
    name : Str,
    type : TypeName,
    arguments : List Argument,
    resolve : Dict Str Value -> Result Value Gql.Input.Error,
}

TypeName : [
    String,
    Int,
]

Type a : {
    type : TypeName,
    encode : a -> Value,
}

field :
    Str,
    Type output,
    {
        takes : Input input,
        resolve : input -> output,
    }
    -> Field
field = \name, returns, { takes, resolve } -> {
    name,
    type: returns.type,
    arguments: Gql.Input.arguments takes,
    resolve: \inputValue ->
        inputValue
        |> Gql.Input.decode takes
        |> Result.map \input ->
            input
            |> resolve
            |> returns.encode,
}

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
            InputErr Gql.Input.Error,
            OperationNotFound,
            VarNotFound Str,
        ]
execute = \params ->
    operation <-
        params.document
        |> Gql.Document.findOperation params.operation
        |> Result.try

    obj = params.schema.query

    # TODO: Err if var is not used
    # TODO: Err if argument used undefined var
    # TODO: Err if var type doesn't match argument type

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
        object "Query" [
            field "greet" string {
                takes: const {
                    name: <- optional "name" Gql.Input.string,
                },
                resolve: \{ name } ->
                    "Hi, \(Result.withDefault name "friend")!",
            },
            field "plus" int {
                takes: const {
                    a: <- required "a" Gql.Input.int,
                    b: <- required "b" Gql.Input.int,
                },
                resolve: \{ a, b } -> a + b,
            },
        ]

    mySchema = schema { query }

    result =
        document <-
            parseDocument
                """
                query Test($name: String) {
                    greet
                    greetWithName: greet(name: $name)
                    plus(a: 2, b: 3)
                }
                """
            |> Result.try

        execute {
            schema: mySchema,
            document,
            operation: First,
            variables: Dict.fromList [("name", String "Matt")],
        }

    result
    == Ok
        (
            Object [
                ("greet", String "Hi, friend!"),
                ("greetWithName", String "Hi, Matt!"),
                ("plus", Int 5),
            ]
        )


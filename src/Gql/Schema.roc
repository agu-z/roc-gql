interface Gql.Schema
    exposes [execute]
    imports [
        Gql.Document.{ Document, Selection },
        Gql.Parse.{ parseDocument },
        Gql.Value.{ Value },
        Gql.Output.{
            ResolveErr,
            Object,
            string,
            int,
            listOf,
            nullable,
            ref,
            object,
            field,
            resolveObject,
        },
        Gql.Input.{ const, required, optional },
        Gql.Enum,
    ]

execute :
    {
        schema : schema,
        queryRoot : Object root schema,
        document : Document,
        operation : [First, ByName Str],
        variables : Dict Str Value,
        rootValue : root,
    }
    -> Result Value [OperationNotFound, ResolveErr ResolveErr]
execute = \params ->
    operation <-
        params.document
        |> Gql.Document.findOperation params.operation
        |> Result.try

    # TODO: Err if var is not used
    # TODO: Err if argument used undefined var
    # TODO: Err if var type doesn't match argument type
    # TODO: Mutation

    (params.queryRoot)
    |> resolveObject
        params.rootValue
        params.schema
        operation.selectionSet
        params.variables
    |> Result.mapErr ResolveErr

expect
    query =
        object "Query" [
            field "greet" string {
                takes: const {
                    name: <- optional "name" Gql.Input.string,
                },
                resolve: \_, { name } ->
                    "Hi, \(Result.withDefault name "friend")!",
            },
            field "plus" int {
                takes: const {
                    a: <- required "a" Gql.Input.int,
                    b: <- required "b" Gql.Input.int,
                },
                resolve: \_, { a, b } -> a + b,
            },
        ]

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
            schema: {},
            queryRoot: query,
            document,
            operation: First,
            variables: Dict.fromList [("name", String "Matt")],
            rootValue: {},
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


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
        schema : {
            query : Object root,
        },
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

    params.schema.query
    |> resolveObject
        params.rootValue
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
            schema: { query },
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

expect
    query =
        object "Query" [
            field "lastOrder" (ref order) {
                takes: const {},
                resolve: \{}, {} -> {
                    id: 1,
                    status: Placed,
                    products: [
                        { id: 1, name: "Pencil", description: Ok "To write stuff", stock: 30 },
                        { id: 2, name: "Notebook", description: Err Nothing, stock: 23 },
                        { id: 3, name: "Ruler", description: Err Nothing, stock: 15 },
                    ],
                },
            },
        ]

    order =
        object "Order" [
            field "id" int { takes: const {}, resolve: \o, _ -> o.id },
            field "status" orderStatus { takes: const {}, resolve: \o, _ -> o.status },
            field "products" (listOf (ref product)) { takes: const {}, resolve: \o, _ -> o.products },
        ]

    orderStatus =
        Gql.Enum.new "OrderStatus" {
            placed: <- Gql.Enum.case "PLACED",
            delivered: <- Gql.Enum.case "DELIVERED",
        }
        |> Gql.Enum.type \value ->
            when value is
                Placed ->
                    .placed

                Delivered ->
                    .delivered

    product =
        object "Product" [
            field "id" int { takes: const {}, resolve: \p, _ -> p.id },
            field "name" string { takes: const {}, resolve: \p, _ -> p.name },
            field "description" (nullable string) { takes: const {}, resolve: \p, _ -> p.description },
            field "stock" int { takes: const {}, resolve: \p, _ -> p.stock },
        ]

    result =
        document <-
            parseDocument
                """
                query {
                    lastOrder {
                        id
                        status
                        products {
                            id
                            name
                            description
                            stock
                        }
                    }
                }
                """
            |> Result.try

        execute {
            schema: { query },
            document,
            operation: First,
            variables: Dict.empty {},
            rootValue: {},
        }

    expectedProducts =
        List [
            Object [
                ("id", Int 1),
                ("name", String "Pencil"),
                ("description", String "To write stuff"),
                ("stock", Int 30),
            ],
            Object [
                ("id", Int 2),
                ("name", String "Notebook"),
                ("description", Null),
                ("stock", Int 23),
            ],
            Object [
                ("id", Int 3),
                ("name", String "Ruler"),
                ("description", Null),
                ("stock", Int 15),
            ],
        ]

    expected = Object [
        (
            "lastOrder",
            Object [
                ("id", Int 1),
                ("status", Enum "PLACED"),
                ("products", expectedProducts),
            ],
        ),
    ]

    result == Ok expected

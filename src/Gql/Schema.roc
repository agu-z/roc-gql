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

    params.schema
    |> addIntrospectionSchema
    |> .query
    |> resolveObject
        params.rootValue
        operation.selectionSet
        params.variables
    |> Result.mapErr ResolveErr

addIntrospectionSchema : { query : Object a } -> { query : Object a }
addIntrospectionSchema = \{ query } ->
    types =
        Dict.withCapacity 10
        |> gatherTypes query.meta.fields

    return = \fn -> { takes: const {}, resolve: \v, _ -> fn v }

    typeObj = \{ fieldRef } ->
        object "__Type" [
            field "kind" typeKind (return .kind),
            field "name" (nullable string) (return .name),
            field "description" (nullable string) (return .description),
            field "fields" (nullable (listOf (ref fieldRef))) (return .fields),
            # NOT IMPLEMENTED:
            # Only here so that query doesn't fail
            field "inputFields" (nullable string) (return .inputFields), #
            field "interfaces" (nullable string) (return .interfaces),
            field "possibleTypes" (nullable string) (return .possibleTypes),
            field "enumValues" (nullable string) (return .enumValues),
        ]

    typeKind =
        Gql.Enum.new "__TypeKind" {
            scalar: <- Gql.Enum.case "SCALAR",
            object: <- Gql.Enum.case "OBJECT",
            interface: <- Gql.Enum.case "INTERFACE",
            union: <- Gql.Enum.case "UNION",
            enum: <- Gql.Enum.case "ENUM",
            inputObject: <- Gql.Enum.case "INPUT_OBJECT",
            list: <- Gql.Enum.case "LIST",
            nonNull: <- Gql.Enum.case "NON_NULL",
        }
        |> Gql.Enum.type \value -> value

    fieldObj =
        object "__Field" [
            field "name" string (return .name),
            field "type" typeRef (return .type),
        ]

    # Compiler bugs prevent us from having a normal recursive object
    # For now, we're just returning fields relevant to clients regardless of selection
    typeRef = {
        type: Ref { name: "__Type", fields: [] },
        resolve: \t, _, _ -> Ok (encodeTypeRef t),
    }

    encodeTypeRef = \type ->
        encodeNonNull = \t ->
            when t is
                String ->
                    Object [
                        ("kind", Enum "SCALAR"),
                        ("name", String "String"),
                        ("ofType", Null),
                    ]

                Int ->
                    Object [
                        ("kind", Enum "SCALAR"),
                        ("name", String "Int"),
                        ("ofType", Null),
                    ]

                List itemType ->
                    Object [
                        ("kind", Enum "LIST"),
                        ("name", Null),
                        ("ofType", encodeTypeRef itemType),
                    ]

                Nullable _ ->
                    crash "unreachable"

                Ref o ->
                    Object [
                        ("kind", Enum "OBJECT"),
                        ("name", String o.name),
                        ("ofType", Null),
                    ]

                Enum name _ ->
                    Object [
                        ("kind", Enum "ENUM"),
                        ("name", String name),
                        ("ofType", Null),
                    ]

        when type is
            Nullable itemType ->
                encodeNonNull itemType

            _ ->
                Object [
                    ("kind", Enum "NON_NULL"),
                    ("name", Null),
                    ("ofType", encodeNonNull type),
                ]

    typeQueryField =
        field "__type" (nullable (ref (typeObj { fieldRef: fieldObj }))) {
            takes: const {
                name: <- required "name" Gql.Input.string,
            },
            resolve: \_, { name } ->
                Dict.get types name
                |> Result.map \obj -> {
                    kind: .object,
                    name: Ok obj.name,
                    description: Err Nothing,
                    fields: Ok obj.fields,
                    interfaces: Err Nothing,
                    possibleTypes: Err Nothing,
                    enumValues: Err Nothing,
                    inputFields: Err Nothing,
                    ofType: Err Nothing,
                }
                |> Result.mapErr \KeyNotFound -> Nothing,
        }

    queryMeta = query.meta

    {
        query: { query &
            meta: { queryMeta &
                fields: queryMeta.fields
                |> List.append typeQueryField.meta,
            },
            resolvers: query.resolvers
            |> Dict.insert typeQueryField.meta.name typeQueryField.resolve,
        },
    }

gatherTypes = \dict, fields ->
    cdict, cfield <- List.walk fields dict

    when getRef cfield.type is
        Ok obj ->
            cdict
            |> Dict.insert obj.name obj
            |> gatherTypes obj.fields

        Err NotRef ->
            cdict

getRef : Gql.Output.TypeMeta -> Result Gql.Output.ObjectMeta [NotRef]
getRef = \type ->
    when type is
        Ref obj ->
            Ok obj

        List ltype ->
            getRef ltype

        Nullable ntype ->
            getRef ntype

        String | Int | Enum _ _ ->
            Err NotRef

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

testSchema =
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

    { query }

expect
    result =
        document <-
            parseDocument
                """
                query {
                    lastOrder {
                        __typename
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
            schema: testSchema,
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
                ("__typename", String "Order"),
                ("id", Int 1),
                ("status", Enum "PLACED"),
                ("products", expectedProducts),
            ],
        ),
    ]

    result == Ok expected

expect
    result =
        document <-
            parseDocument
                """
                query {
                    order: __type(name: "Order") {
                        kind
                        name
                        fields {
                            name
                            type {
                                kind
                                name
                                ofType {
                                    kind
                                    name
                                }
                            }
                        }
                    }
                }
                """
            |> Result.try

        execute {
            schema: testSchema,
            document,
            operation: First,
            variables: Dict.empty {},
            rootValue: {},
        }

    expected =
        Object [
            (
                "order",
                Object [
                    ("kind", Enum "OBJECT"),
                    ("name", String "Order"),
                    (
                        "fields",
                        List [
                            Object [
                                ("name", String "id"),
                                (
                                    "type",
                                    Object [
                                        ("kind", Enum "NON_NULL"),
                                        ("name", Null),
                                        (
                                            "ofType",
                                            Object [
                                                ("kind", Enum "SCALAR"),
                                                ("name", String "Int"),
                                                ("ofType", Null),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                            Object [
                                ("name", String "status"),
                                (
                                    "type",
                                    Object [
                                        ("kind", Enum "NON_NULL"),
                                        ("name", Null),
                                        (
                                            "ofType",
                                            Object [
                                                ("kind", Enum "ENUM"),
                                                ("name", String "OrderStatus"),
                                                ("ofType", Null),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                            Object [
                                ("name", String "products"),
                                (
                                    "type",
                                    Object [
                                        ("kind", Enum "NON_NULL"),
                                        ("name", Null),
                                        (
                                            "ofType",
                                            Object [
                                                ("kind", Enum "LIST"),
                                                ("name", Null),
                                                (
                                                    "ofType",
                                                    Object [
                                                        ("kind", Enum "NON_NULL"),
                                                        ("name", Null),
                                                        (
                                                            "ofType",
                                                            Object [
                                                                ("kind", Enum "OBJECT"),
                                                                ("name", String "Product"),
                                                                ("ofType", Null),
                                                            ],
                                                        ),
                                                    ],
                                                ),
                                            ],
                                        ),
                                    ],
                                ),
                            ],
                        ],
                    ),
                ],
            ),
        ]

    result == Ok expected

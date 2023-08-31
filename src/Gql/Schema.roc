interface Gql.Schema
    exposes [execute, inputTestSchema]
    imports [
        Gql.Document.{ Document, Selection },
        Gql.Parse.{ parseDocument },
        Gql.Value.{ Value },
        Gql.Output.{
            ResolveErr,
            Object,
            ObjectMeta,
            EnumMeta,
            FieldMeta,
            string,
            int,
            boolean,
            listOf,
            nullable,
            ref,
            object,
            field,
            resolveObject,
        },
        Gql.Docs.{ describe, deprecate },
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
    -> Result
        Value
        [
            OperationNotFound,
            FragmentNotFound Str,
            RecursiveFragment Str,
            ResolveErr ResolveErr,
        ]
execute = \params ->
    operation <-
        params.document
        |> Gql.Document.findOperation params.operation
        |> Result.try

    # TODO: Err if var is not used
    # TODO: Err if argument used undefined var
    # TODO: Err if var type doesn't match argument type

    canSelection <-
        operation.selectionSet
        |> List.mapTry \sel -> Gql.Document.canSelection sel params.document
        |> Result.map List.join
        |> Result.try

    params.schema
    |> addIntrospectionSchema
    |> .query
    |> resolveObject params.rootValue canSelection {
        variables: params.variables,
        document: params.document,
    }
    |> Result.mapErr ResolveErr

addIntrospectionSchema : { query : Object a } -> { query : Object a }
addIntrospectionSchema = \{ query } ->
    # TODO: Spec compliant
    # TODO: Include introspection schema itself

    queryMeta = Gql.Output.objectMeta query

    types =
        Dict.withCapacity 32
        |> Dict.insert queryMeta.name (Object queryMeta)
        |> Dict.insert "String" (Scalar "String")
        |> Dict.insert "Int" (Scalar "Int")
        |> Dict.insert "Boolean" (Scalar "Boolean")
        |> gatherNamedTypes queryMeta.fields

    return = \fn -> { takes: const {}, resolve: \v, _ -> fn v }

    namedType =
        object "__Type" [
            field "kind" typeKind (return .kind),
            field "name" (nullable string) (return .name),
            field "description" (nullable string) (return .description),
            field "fields" (nullable (listOf (ref fieldObj))) (return .fields),
            field "enumValues" (nullable (listOf (ref enumValue))) (return .enumValues),
            # NOT IMPLEMENTED:
            # Only here so that query doesn't fail
            field "inputFields" (nullable string) (return \_ -> Err Nothing), #
            field "interfaces" (nullable (listOf string)) (return \_ -> Ok []),
            field "possibleTypes" (nullable string) (return \_ -> Err Nothing),
        ]

    enumValue : Object Gql.Output.EnumValue
    enumValue =
        object "__EnumValue" [
            field "name" string (return .name),
            # NOT IMPLEMENTED:
            field "description" (nullable string) (return \_ -> Err Nothing),
            field "isDeprecated" boolean (return \_ -> Bool.false),
            field "deprecationReason" (nullable string) (return \_ -> Err Nothing),
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
            field "args" (listOf (ref inputValue)) (return .arguments),
            field "description" (nullable string) (return .description),
            field "isDeprecated" boolean (return \f -> Result.isOk f.deprecationReason),
            field "deprecationReason" (nullable string) (return .deprecationReason),
        ]

    # Compiler bugs prevent us from having a normal recursive object
    # For now, we're just returning fields relevant to clients regardless of selection
    typeRef = {
        type: Ref {
            name: "__Type",
            description: Err Nothing,
            fields: [],
        },
        resolve: \t, _, _ -> Ok (encodeTypeRef t),
    }

    encodeTypeRef : Gql.Output.TypeMeta -> Value
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

                Boolean ->
                    Object [
                        ("kind", Enum "SCALAR"),
                        ("name", String "Boolean"),
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

                Enum e ->
                    Object [
                        ("kind", Enum "ENUM"),
                        ("name", String e.name),
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

    inputValue : Object Gql.Input.Argument
    inputValue =
        object "__InputValue" [
            field "name" string (return .name),
            field "type" inputTypeRef (return .type),
            # NOT IMPLEMENTED:
            field "description" (nullable string) (return \_ -> Err Nothing),
            field "defaultValue" (nullable string) (return \_ -> Err Nothing),
        ]

    # TODO: Should we unify input & output types somehow?
    inputTypeRef : Gql.Output.Type Gql.Input.TypeMeta
    inputTypeRef = {
        type: Ref {
            name: "__Type",
            description: Err Nothing,
            fields: [],
        },
        resolve: \t, _, _ -> Ok (encodeInputTypeRef t),
    }

    encodeInputTypeRef = \type ->
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

                Nullable _ ->
                    crash "unreachable"

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
        field "__type" (nullable (ref namedType)) {
            takes: const {
                name: <- required "name" Gql.Input.string,
            },
            resolve: \_, { name } ->
                Dict.get types name
                |> Result.map namedTypeToRecord
                |> Result.mapErr \KeyNotFound -> Nothing,
        }

    # TODO: Do this at namedType object
    namedTypeToRecord = \type ->
        when type is
            Scalar name ->
                {
                    kind: .scalar,
                    name: Ok name,
                    description: Err Nothing,
                    fields: Err Nothing,
                    enumValues: Err Nothing,
                }

            Object obj ->
                {
                    kind: .object,
                    name: Ok obj.name,
                    description: obj.description,
                    fields: Ok obj.fields,
                    enumValues: Err Nothing,
                }

            Enum enum ->
                {
                    kind: .enum,
                    name: Ok enum.name,
                    description: Err Nothing,
                    fields: Err Nothing,
                    enumValues: Ok enum.values,
                }

    schemaObj =
        object "__Schema" [
            field "types" (listOf (ref namedType)) {
                takes: const {},
                resolve: \schema, _ ->
                    schema.types
                    |> Dict.toList
                    |> List.map \(_, type) -> namedTypeToRecord type,
            },
            field "queryType" (ref namedType) (return \_ -> queryType),
            # NOT IMPLEMENTED:
            field "description" (nullable string) (return \_ -> Err Nothing),
            field "mutationType" (nullable (ref namedType)) (return \_ -> Err Nothing),
            field "subscriptionType" (nullable (ref namedType)) (return \_ -> Err Nothing),
            field "directives" (listOf string) (return \_ -> []),
        ]

    # For some reason, I need to define this outside of schemaObj
    # Otherwise I get "Error in alias analysis"
    queryType = namedTypeToRecord (Object queryMeta)

    schemaQueryField =
        field "__schema" (ref schemaObj) {
            takes: const {},
            resolve: \_, _ -> { types },
        }

    {
        query: query
        |> Gql.Output.addField typeQueryField
        |> Gql.Output.addField schemaQueryField,
    }

NamedType : [
    Scalar Str,
    Object ObjectMeta,
    Enum EnumMeta,
    # InputObject, Interface, Union, Scalar
]

gatherNamedTypes : Dict Str NamedType, List FieldMeta -> Dict Str NamedType
gatherNamedTypes = \dict, fields ->
    cdict, cfield <- List.walk fields dict

    when getNamedType cfield.type is
        Ok (Object obj) ->
            cdict
            |> Dict.insert obj.name (Object obj)
            |> gatherNamedTypes obj.fields

        Ok (Enum enum) ->
            Dict.insert cdict enum.name (Enum enum)

        Ok (Scalar name) ->
            Dict.insert cdict name (Scalar name)

        Err Unnamed ->
            cdict

getNamedType : Gql.Output.TypeMeta -> Result NamedType [Unnamed]
getNamedType = \type ->
    when type is
        Ref obj ->
            Ok (Object obj)

        Enum enum ->
            Ok (Enum enum)

        List ltype ->
            getNamedType ltype

        Nullable ntype ->
            getNamedType ntype

        String | Int | Boolean ->
            Err Unnamed

# TESTS

inputTestSchema =
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
            }
            |> describe "Add two numbers",
        ]

    { query }

expect
    result =
        testQuery {
            schema: inputTestSchema,
            query:
            """
            query Test($name: String) {
                greet
                greetWithName: greet(name: $name)
                plus(a: 2, b: 3)
            }
            """,
            variables: Dict.fromList [("name", String "Matt")],
            path: [],
        }

    expected =
        Object [
            ("greet", String "Hi, friend!"),
            ("greetWithName", String "Hi, Matt!"),
            ("plus", Int 5),
        ]

    result == Ok expected

refsTestSchema =
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
        testQuery {
            schema: refsTestSchema,
            query:
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
            """,
            path: [Key "lastOrder"],
        }

    expected =
        Object [
            ("__typename", String "Order"),
            ("id", Int 1),
            ("status", Enum "PLACED"),
            ("products", expectedProducts),
        ]

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

    result == Ok expected

# FRAGMENT SPREAD

expect
    result =
        testQuery {
            schema: refsTestSchema,
            query:
            """
            query {
                lastOrder {
                    __typename
                    ...Order
                    products {
                        ...Product
                    }
                }
            }

            fragment Order on Order {
                id
                status
            }

            fragment Product on Product {
                id
                name
                description
                stock
            }
            """,
            path: [Key "lastOrder"],
        }

    expected = Object [
        ("__typename", String "Order"),
        ("id", Int 1),
        ("status", Enum "PLACED"),
        ("products", expectedProducts),
    ]

    expectedProducts = List [
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

    result == Ok expected

# INTROSPECTION TESTS

# __type for object
expect
    result =
        testQuery {
            schema: refsTestSchema,
            query:
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
                                ofType {
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
                }
            }
            """,
            path: [Key "order"],
        }

    expected =
        Object [
            ("kind", Enum "OBJECT"),
            ("name", String "Order"),
            (
                "fields",
                List [
                    Object nameField,
                    Object statusField,
                    Object productsField,
                ],
            ),
        ]

    nameField = [
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
    ]

    statusField = [
        ("name", String "status"),
        (
            "type",
            Object [
                ("kind", Enum "ENUM"),
                ("name", String "OrderStatus"),
                ("ofType", Null),
            ]
            |> nonNullObj,
        ),
    ]

    productsField = [
        ("name", String "products"),
        (
            "type",
            Object [
                ("kind", Enum "OBJECT"),
                ("name", String "Product"),
                ("ofType", Null),
            ]
            |> nonNullObj
            |> listObj
            |> nonNullObj,
        ),
    ]

    nonNullObj = \t ->
        Object [
            ("kind", Enum "NON_NULL"),
            ("name", Null),
            ("ofType", t),
        ]

    listObj = \t ->
        Object [
            ("kind", Enum "LIST"),
            ("name", Null),
            ("ofType", t),
        ]

    result == Ok expected

# __type for enum
expect
    result =
        testQuery {
            schema: refsTestSchema,
            query:
            """
            {
                orderStatus: __type(name: "OrderStatus") {
                    kind
                    name
                    enumValues {
                        name
                    }
                }
            }
            """,
            path: [Key "orderStatus"],
        }

    expected =
        Object [
            ("kind", Enum "ENUM"),
            ("name", String "OrderStatus"),
            (
                "enumValues",
                List [
                    Object [("name", String "PLACED")],
                    Object [("name", String "DELIVERED")],
                ],
            ),
        ]

    result == Ok expected

## field arguments
expect
    result =
        testQuery {
            schema: inputTestSchema,
            query:
            """
            query {
                query: __type(name: "Query") {
                    fields {
                        name
                        args {
                            name
                            type {
                                kind
                                name
                                ofType {
                                    kind
                                    name
                                    ofType {
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
                    }
                }
            }
            """,
            path: [Key "query", Key "fields"],
        }

    expected =
        List [
            Object [
                ("name", String "greet"),
                ("args", List greetArgs),
            ],
            Object [
                ("name", String "plus"),
                ("args", List plusArgs),
            ],
        ]

    greetArgs = [
        Object [
            ("name", String "name"),
            (
                "type",
                Object [
                    ("kind", Enum "SCALAR"),
                    ("name", String "String"),
                    ("ofType", Null),
                ],
            ),
        ],
    ]

    plusArgs = [
        Object [
            ("name", String "a"),
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
            ("name", String "b"),
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
    ]

    result == Ok expected

# __schema
expect
    result =
        testQuery {
            schema: refsTestSchema,
            query:
            """
            {
                __schema {
                    queryType {
                        name
                    }
                    types {
                        name
                        fields {
                            name
                        }
                        enumValues {
                            name
                        }
                    }
                }
            }
            """,
            path: [Key "__schema"],
        }

    expected =
        Object [
            ("queryType", Object [("name", String "Query")]),
            ("types", List expectedTypes),
        ]

    expectedTypes = [
        Object [
            ("name", String "Query"),
            ("fields", List [Object [("name", String "lastOrder")]]),
            ("enumValues", Null),
        ],
        Object [
            ("name", String "String"),
            ("fields", Null),
            ("enumValues", Null),
        ],
        Object [
            ("name", String "Int"),
            ("fields", Null),
            ("enumValues", Null),
        ],
        Object [
            ("name", String "Boolean"),
            ("fields", Null),
            ("enumValues", Null),
        ],
        Object [
            ("name", String "Order"),
            (
                "fields",
                List [
                    Object [("name", String "id")],
                    Object [("name", String "status")],
                    Object [("name", String "products")],
                ],
            ),
            ("enumValues", Null),
        ],
        Object [
            ("name", String "OrderStatus"),
            ("fields", Null),
            (
                "enumValues",
                List [
                    Object [("name", String "PLACED")],
                    Object [("name", String "DELIVERED")],
                ],
            ),
        ],
        Object [
            ("name", String "Product"),
            (
                "fields",
                List [
                    Object [("name", String "id")],
                    Object [("name", String "name")],
                    Object [("name", String "description")],
                    Object [("name", String "stock")],
                ],
            ),
            ("enumValues", Null),
        ],
    ]

    result == Ok expected

# DOCS:

docsTestSchema =
    query =
        object "Query" [
            field "me" (ref user) {
                takes: const {},
                resolve: \_, _ -> { name: "John" },
            }
            |> describe "The currently logged in user"
            |> deprecate "Use Query.user instead",
        ]
        |> describe "All the queries"

    user =
        object "User" [
            field "name" string { takes: const {}, resolve: \u, _ -> u.name },
        ]
        |> describe "A person or bot"

    { query }

# Objects
expect
    result = testQuery {
        schema: docsTestSchema,
        query:
        """
        {
            user: __type(name: "User") {
                description
            }
        }
        """,
        path: [Key "user", Key "description"],
    }

    result == Ok (String "A person or bot")

expect
    result = testQuery {
        schema: docsTestSchema,
        query:
        """
        {
            __schema {
                queryType {
                    description
                }
            }
        }
        """,
        path: [Key "__schema", Key "queryType", Key "description"],
    }

    result == Ok (String "All the queries")

# Object Fields
expect
    result = testQuery {
        schema: docsTestSchema,
        query:
        """
        {
            __schema {
                queryType {
                    fields {
                        name
                        description
                        isDeprecated
                        deprecationReason
                    }
                }
            }
        }
        """,
        path: [Key "__schema", Key "queryType", Key "fields", Index 0],
    }

    expected =
        Object [
            ("name", String "me"),
            ("description", String "The currently logged in user"),
            ("isDeprecated", Boolean Bool.true),
            ("deprecationReason", String "Use Query.user instead"),
        ]

    result == Ok expected

# Test helpers

testQuery = \{ schema, query, path, variables ? Dict.empty {} } ->
    document <- parseDocument query |> Result.try

    execute {
        schema,
        document,
        operation: First,
        variables,
        rootValue: {},
    }
    |> Result.try
        \val -> Gql.Value.get val path


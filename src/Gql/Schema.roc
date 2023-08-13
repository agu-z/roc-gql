interface Gql.Schema
    exposes [
        Type,
        Field,
        TypeName,
        Object,
        object,
        field,
        int,
        string,
        execute,
    ]
    imports [
        Gql.Document.{ Document, Selection },
        Gql.Parse.{ parseDocument },
        Gql.Input.{ Input, Argument, const, optional, required },
        Gql.Value.{ Value },
    ]

Object a : {
    name : Str,
    fields : Dict Str (Field a),
}

object : Str, List (Field a) -> Object a
object = \name, fields -> {
    name,
    fields: fields
    |> List.map \f -> (f.name, f)
    |> Dict.fromList,
}

Field a : {
    name : Str,
    type : TypeName,
    arguments : List Argument,
    resolve : a, Dict Str Value, List Selection, Dict Str Value -> Result Value ResolveErr,
}

ResolveErr : [
    FieldNotFound Str Str,
    InputErr Gql.Input.Error,
    VarNotFound Str,
]

TypeName : [
    String,
    Int,
    List TypeName,
    Ref Str,
]

Type a : {
    type : TypeName,
    resolve : a, List Selection, Dict Str Value -> Result Value ResolveErr,
}

field :
    Str,
    Type output,
    {
        takes : Input input,
        resolve : a, input -> output,
    }
    -> Field a
field = \name, returns, { takes, resolve } -> {
    name,
    type: returns.type,
    arguments: Gql.Input.arguments takes,
    resolve: \obj, args, selection, vars ->
        args
        |> Gql.Input.decode takes
        |> Result.mapErr InputErr
        |> Result.try \input ->
            returns.resolve (resolve obj input) selection vars,
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

listOf : Type a -> Type (List a)
listOf = \itemType -> {
    type: List itemType.type,
    resolve: \list, selection, vars ->
        list
        |> List.mapTry \item -> itemType.resolve item selection vars
        |> Result.map List,
}

ref : Object a -> Type a
ref = \obj -> {
    type: Ref obj.name,
    resolve: \value, selection, vars -> resolveObject obj value selection vars,
}

resolveObject : Object a, a, List Selection, Dict Str Value -> Result Value ResolveErr
resolveObject = \obj, a, selectionSet, vars ->
    selectionSet
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
                        |> Gql.Value.fromDocument vars
                        |> Result.map \value -> (key, value)
                    |> Result.map Dict.fromList
                    |> Result.try

                value <- schemaField.resolve a argsDict opField.selectionSet vars
                    |> Result.map

                (outName, value)

            _ ->
                crash "todo"
    |> Result.map Object

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
                    products: [
                        { id: 1, name: "Pencil", stock: 30 },
                        { id: 2, name: "Notebook", stock: 23 },
                        { id: 3, name: "Ruler", stock: 15 },
                    ],
                },
            },
        ]

    order =
        object "Order" [
            field "id" int { takes: const {}, resolve: \o, _ -> o.id },
            field "products" (listOf (ref product)) { takes: const {}, resolve: \o, _ -> o.products },
        ]

    product =
        object "Product" [
            field "id" int { takes: const {}, resolve: \p, _ -> p.id },
            field "name" string { takes: const {}, resolve: \p, _ -> p.name },
            field "stock" int { takes: const {}, resolve: \p, _ -> p.stock },
        ]

    result =
        document <-
            parseDocument
                """
                query {
                    lastOrder {
                        id
                        products {
                            id
                            name
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
                ("stock", Int 30),
            ],
            Object [
                ("id", Int 2),
                ("name", String "Notebook"),
                ("stock", Int 23),
            ],
            Object [
                ("id", Int 3),
                ("name", String "Ruler"),
                ("stock", Int 15),
            ],
        ]

    expected = Object [
        (
            "lastOrder",
            Object [
                ("id", Int 1),
                ("products", expectedProducts),
            ],
        ),
    ]

    result == Ok expected

interface Gql.Value
    exposes [Value, fromDocument, maybe, toJson, get]
    imports [Gql.Document]

Value : [
    Int I32,
    String Str,
    Boolean Bool,
    Null,
    Enum Str,
    List (List Value),
    Object (List (Str, Value)),
    # TODO:
    # FloatValue
]

fromDocument : Gql.Document.Value, Dict Str Value -> Result Value [VarNotFound Str]
fromDocument = \docValue, variables ->
    when docValue is
        Var name ->
            variables
            |> Dict.get name
            |> Result.mapErr \KeyNotFound -> VarNotFound name

        Int value ->
            Ok (Int value)

        String value ->
            Ok (String value)

        Boolean value ->
            Ok (Boolean value)

        Null ->
            Ok Null

        Enum value ->
            Ok (Enum value)

        List list ->
            list
            |> List.mapTry \value -> fromDocument value variables
            |> Result.map List

        Object object ->
            object
            |> List.mapTry \(key, value) ->
                value
                |> fromDocument variables
                |> Result.map \mappedValue -> (key, mappedValue)
            |> Result.map Object

expect fromDocument (Int 3) (Dict.empty {}) == Ok (Int 3)
expect fromDocument (Var "name") (Dict.empty {}) == Err (VarNotFound "name")
expect fromDocument (Var "name") (Dict.fromList [("name", String "Jake")]) == Ok (String "Jake")
expect
    fromDocument
        (
            List [
                Int 123,
                Var "value",
            ]
        )
        (Dict.fromList [("value", Int 321)])
    == Ok
        (
            List [
                Int 123,
                Int 321,
            ]
        )
expect
    fromDocument
        (
            Object [
                ("first", Int 123),
                ("after", Var "cursor"),
            ]
        )
        (Dict.fromList [("cursor", String "abc")])
    == Ok
        (
            Object [
                ("first", Int 123),
                ("after", String "abc"),
            ]
        )

maybe : Result a [Nothing], (a -> Value) -> Value
maybe = \m, fn ->
    when m is
        Ok val ->
            fn val

        Err Nothing ->
            Null

expect maybe (Ok "hi") String == String "hi"
expect maybe (Err Nothing) String == Null

get : Value, List [Key Str, Index Nat] -> Result Value [NotFound]
get = \value, path ->
    { before, others } = List.split path 1

    when (before, value) is
        ([], _) ->
            Ok value

        ([Key wantedKey], Object fields) ->
            fields
            |> List.findFirst \(key, _) -> key == wantedKey
            |> Result.try \(_, sub) -> get sub others

        ([Index index], List items) ->
            items
            |> List.get index
            |> Result.mapErr \OutOfBounds -> NotFound
            |> Result.try \sub -> get sub others

        _ ->
            Err NotFound

expect String "hi" |> get [] == Ok (String "hi")
expect List [Int 0, Int 1, Int 2] |> get [Index 1] == Ok (Int 1)
expect
    Object [
        ("zero", Int 0),
        ("one", Int 1),
    ]
    |> get [Key "zero"]
    == Ok (Int 0)
expect
    Object [
        ("zero", Int 0),
        ("one", Int 1),
    ]
    |> get [Key "one"]
    == Ok (Int 1)
expect
    Object [
        ("even", List [Int 0, Int 2]),
        ("odd", List [Int 1, Int 3]),
    ]
    |> get [Key "odd"]
    == Ok (List [Int 1, Int 3])
expect
    Object [
        ("even", List [Int 0, Int 2]),
        ("odd", List [Int 1, Int 3]),
    ]
    |> get [Key "even", Index 1]
    == Ok (Int 2)
expect Object [("zero", Int 0), ("one", Int 1)] |> get [Index 0] == Err NotFound

## JSON ENCODING
# This is 100% a proof of concept encoder which doesn't even escape strings.
# Do not take seriously.

toJson : Value -> Str
toJson = \val ->
    when val is
        Int int ->
            Num.toStr int

        String str | Enum str ->
            strToJson str

        Boolean bool ->
            if bool then
                "true"
            else
                "false"

        Null ->
            "null"

        List values ->
            items =
                values
                |> List.map toJson
                |> Str.joinWith ","

            "[\(items)]"

        Object fields ->
            items =
                fields
                |> List.map \(key, value) ->
                    "\(strToJson key):\(toJson value)"
                |> Str.joinWith ","

            "{\(items)}"

strToJson : Str -> Str
strToJson = \str ->
    # TODO: Escape
    "\"\(str)\""

expect
    input =
        Object [
            (
                "orderStatus",
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
                ],
            ),
        ]

    expected =
        """
        {"orderStatus":{"kind":"ENUM","name":"OrderStatus","enumValues":[{"name":"PLACED"},{"name":"DELIVERED"}]}}
        """

    result = toJson input

    expected == result

expect
    products =
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

    input = Object [
        (
            "lastOrder",
            Object [
                ("__typename", String "Order"),
                ("id", Int 1),
                ("status", Enum "PLACED"),
                ("products", products),
            ],
        ),
    ]

    expected =
        """
        {"lastOrder":{"__typename":"Order","id":1,"status":"PLACED","products":[{"id":1,"name":"Pencil","description":"To write stuff","stock":30},{"id":2,"name":"Notebook","description":null,"stock":23},{"id":3,"name":"Ruler","description":null,"stock":15}]}}
        """

    result = toJson input

    expected == result


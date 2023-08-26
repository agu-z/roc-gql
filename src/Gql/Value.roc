interface Gql.Value
    exposes [Value, fromDocument, maybe]
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

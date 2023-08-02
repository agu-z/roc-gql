interface Gql.Value
    exposes [Value]
    imports []

Value : [
    String Str,
    Integer I32,
    Null,
    Object (List (Str, Value)),
]

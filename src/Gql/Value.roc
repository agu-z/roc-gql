interface Gql.Value
    exposes [Value]
    imports []

Value : [
    String Str,
    Int I32,
    Null,
    Object (List (Str, Value)),
]

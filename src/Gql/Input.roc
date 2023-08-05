interface Gql.Input
    exposes [
        Argument,
        Input,
        Type,
        Error,
        arguments,
        decode,
        const,
        required,
        optional,
        string,
        int,
    ]
    imports [
        Gql.Value.{ Value },
    ]

Input a := {
    decoder : Dict Str Value -> Result a Error,
    arguments : List Argument,
}

Error : {
    argument : Str,
    problem : [
        Missing,
        NullValue,
        InvalidValue TypeName Value,
    ],
}

TypeName : [
    String,
    Int,
]

Argument : {
    name : Str,
    type : TypeName,
}

const : a -> Input a
const = \value ->
    @Input {
        decoder: \_ -> Ok value,
        arguments: [],
    }

Type a : {
    type : TypeName,
    decoder : Value -> Result a Value,
}

required : Str, Type a -> (Input (a -> b) -> Input b)
required = \name, typeRef ->
    newArg = {
        name,
        type: typeRef.type,
    }

    err = \problem -> { argument: name, problem }

    apply = \@Input fnInput ->
        @Input {
            decoder: \argValues ->
                aValue <-
                    argValues
                    |> Dict.get name
                    |> Result.mapErr \KeyNotFound -> err Missing
                    |> Result.try

                a <- typeRef.decoder aValue
                    |> Result.mapErr \value ->
                        when value is
                            Null ->
                                err NullValue

                            _ ->
                                err (InvalidValue typeRef.type value)
                    |> Result.try

                fn <-
                    fnInput.decoder argValues
                    |> Result.map

                fn a,
            arguments: fnInput.arguments |> List.append newArg,
        }

    apply

optional : Str, Type a -> (Input (Result a [Nothing] -> b) -> Input b)
optional = \name, typeRef ->
    newArg = {
        name,
        type: typeRef.type,
    }

    apply = \@Input fnInput ->
        decoder = \argValues ->
            fn <-
                fnInput.decoder argValues |> Result.try

            when Dict.get argValues name is
                Ok aValue ->
                    when typeRef.decoder aValue is
                        Ok a ->
                            Ok (fn (Ok a))

                        Err Null ->
                            Ok (fn (Err Nothing))

                        Err value ->
                            Err {
                                argument: name,
                                problem: InvalidValue typeRef.type value,
                            }

                Err KeyNotFound ->
                    Ok (fn (Err Nothing))

        @Input {
            decoder,
            arguments: fnInput.arguments |> List.append newArg,
        }

    apply

arguments : Input a -> List Argument
arguments = \@Input input ->
    input.arguments

decode : Dict Str Value, Input a -> Result a Error
decode = \argValues, @Input input ->
    input.decoder argValues

# Types

string : Type Str
string =
    decoder = \value ->
        when value is
            String v ->
                Ok v

            _ ->
                Err value
    { type: String, decoder }

int : Type I32
int =
    decoder = \value ->
        when value is
            Int v ->
                Ok v

            _ ->
                Err value

    { type: Int, decoder }

# Test pipeline

testInput =
    const {
        name: <- required "name" string,
        stock: <- required "stock" int,
    }

expect
    values = Dict.fromList [
        ("name", String "Pencil"),
        ("stock", Int 3000),
    ]

    decode values testInput == Ok { name: "Pencil", stock: 3000 }

expect
    values = Dict.fromList [
        ("name", Int 1),
        ("stock", Int 3000),
    ]

    decode values testInput
    == Err {
        argument: "name",
        problem: InvalidValue String (Int 1),
    }

expect
    values = Dict.fromList [
        ("name", Int 1),
    ]

    decode values testInput == Err { argument: "stock", problem: Missing }

expect
    values = Dict.fromList [
        ("name", Int 1),
        ("stock", Null),
    ]

    decode values testInput == Err { argument: "stock", problem: NullValue }

optionalInput =
    const {
        stock: <- optional "stock" int,
    }

expect
    values = Dict.fromList [("stock", Int 123)]

    decode values optionalInput == Ok { stock: Ok 123 }

expect
    values = Dict.fromList [("stock", Null)]

    decode values optionalInput == Ok { stock: Err Nothing }

expect
    values = Dict.fromList []

    decode values optionalInput == Ok { stock: Err Nothing }

expect
    values = Dict.fromList [("stock", String "123")]

    decode values optionalInput
    == Err {
        argument: "stock",
        problem: InvalidValue Int (String "123"),
    }

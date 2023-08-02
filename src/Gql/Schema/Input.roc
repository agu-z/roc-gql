interface Gql.Schema.Input
    exposes [
        Argument,
        Input,
        Type,
        arguments,
        decode,
        const,
        required,
        optional,
        str,
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
    Integer,
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

str : Type Str
str =
    decoder = \value ->
        when value is
            String string ->
                Ok string

            _ ->
                Err value
    { type: String, decoder }

int : Type I32
int =
    decoder = \value ->
        when value is
            Integer integer ->
                Ok integer

            _ ->
                Err value

    { type: Integer, decoder }

# Test pipeline

testInput =
    const {
        name: <- required "name" str,
        stock: <- required "stock" int,
    }

expect
    values = Dict.fromList [
        ("name", String "Pencil"),
        ("stock", Integer 3000),
    ]

    decode values testInput == Ok { name: "Pencil", stock: 3000 }

expect
    values = Dict.fromList [
        ("name", Integer 1),
        ("stock", Integer 3000),
    ]

    decode values testInput
    == Err {
        argument: "name",
        problem: InvalidValue String (Integer 1),
    }

expect
    values = Dict.fromList [
        ("name", Integer 1),
    ]

    decode values testInput == Err { argument: "stock", problem: Missing }

expect
    values = Dict.fromList [
        ("name", Integer 1),
        ("stock", Null),
    ]

    decode values testInput == Err { argument: "stock", problem: NullValue }

optionalInput =
    const {
        stock: <- optional "stock" int,
    }

expect
    values = Dict.fromList [("stock", Integer 123)]

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
        problem: InvalidValue Integer (String "123"),
    }

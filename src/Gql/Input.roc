interface Gql.Input
    exposes [
        Argument,
        Input,
        TypeMeta,
        Type,
        Error,
        Anonymous,
        arguments,
        decode,
        const,
        required,
        optional,
        none,
        string,
        int,
        boolean,
        object,
    ]
    imports [
        Gql.Value.{ Value },
    ]

Input a name := {
    decoder : Dict Str Value -> Result a Error,
    arguments : List Argument,
    name : name,
}

Error : {
    path : List Str,
    problem : [
        Missing,
        NullValue,
        InvalidValue TypeMeta Value,
    ],
}

TypeMeta : [
    String,
    Int,
    Boolean,
    Nullable TypeMeta,
    Object
        {
            name : Str,
            arguments : List {
                # Can't use aliases because compiler stack overflows
                name : Str,
                type : TypeMeta,
            },
        },
]

Argument : {
    name : Str,
    type : TypeMeta,
}

Anonymous := {}

const : a -> Input a Anonymous
const = \value ->
    @Input {
        decoder: \_ -> Ok value,
        arguments: [],
        name: @Anonymous {},
    }

none : Input {} Anonymous
none = const {}

Type a : {
    type : TypeMeta,
    decoder : Value -> Result a [InvalidValue Value, ObjectFieldErr Error],
}

required : Str, Type a -> (Input (a -> b) name -> Input b name)
required = \name, typeRef ->
    newArg = {
        name,
        type: typeRef.type,
    }

    err = \problem -> { path: [name], problem }

    apply = \@Input fnInput ->
        decoder = \argValues ->
            aValue <-
                argValues
                |> Dict.get name
                |> Result.mapErr \KeyNotFound -> err Missing
                |> Result.try

            a <- typeRef.decoder aValue
                |> Result.mapErr \decodeErr ->
                    when decodeErr is
                        InvalidValue Null ->
                            err NullValue

                        InvalidValue value ->
                            err (InvalidValue typeRef.type value)

                        ObjectFieldErr error ->
                            {
                                path: List.prepend error.path name,
                                problem: error.problem,
                            }
                |> Result.try

            fn <-
                fnInput.decoder argValues
                |> Result.map

            fn a

        @Input {
            decoder,
            arguments: fnInput.arguments |> List.append newArg,
            name: fnInput.name,
        }

    apply

optional : Str, Type a -> (Input (Result a [Nothing] -> b) name -> Input b name)
optional = \name, typeRef ->
    newArg = {
        name,
        type: Nullable typeRef.type,
    }

    apply = \@Input fnInput ->
        decoder = \argValues ->
            fn <- fnInput.decoder argValues |> Result.try

            when Dict.get argValues name is
                Ok aValue ->
                    when typeRef.decoder aValue is
                        Ok a ->
                            Ok (fn (Ok a))

                        Err (InvalidValue Null) ->
                            Ok (fn (Err Nothing))

                        Err (InvalidValue value) ->
                            Err {
                                path: [name],
                                problem: InvalidValue typeRef.type value,
                            }

                        Err (ObjectFieldErr error) ->
                            Err {
                                path: List.prepend error.path name,
                                problem: error.problem,
                            }

                Err KeyNotFound ->
                    Ok (fn (Err Nothing))

        @Input {
            decoder,
            arguments: fnInput.arguments |> List.append newArg,
            name: fnInput.name,
        }

    apply

arguments : Input a * -> List Argument
arguments = \@Input input ->
    input.arguments

decode : Dict Str Value, Input a * -> Result a Error
decode = \argValues, @Input input ->
    # RECONSIDER: Would it be faster to use a List?
    input.decoder argValues

# Objects

Named := Str

object : Str, a -> Input a Named
object = \name, value ->
    @Input {
        decoder: \_ -> Ok value,
        arguments: [],
        name: @Named name,
    }

toType : Input a Named -> Type a
toType = \@Input input ->
    (@Named name) = input.name

    decoder = \value ->
        when value is
            Object fields ->
                fields
                |> Dict.fromList
                |> decode (@Input input)
                |> Result.mapErr ObjectFieldErr

            _ ->
                Err (InvalidValue value)

    { type: Object { name, arguments: input.arguments }, decoder }

# Types

string : Type Str
string =
    decoder = \value ->
        when value is
            String v ->
                Ok v

            _ ->
                Err (InvalidValue value)
    { type: String, decoder }

int : Type I32
int =
    decoder = \value ->
        when value is
            Int v ->
                Ok v

            _ ->
                Err (InvalidValue value)

    { type: Int, decoder }

boolean : Type Bool
boolean =
    decoder = \value ->
        when value is
            Boolean v ->
                Ok v

            _ ->
                Err (InvalidValue value)

    { type: Boolean, decoder }

# Test pipeline

testInput =
    const {
        name: <- required "name" string,
        stock: <- required "stock" int,
        active: <- required "active" boolean,
    }

expect
    values = Dict.fromList [
        ("name", String "Pencil"),
        ("stock", Int 3000),
        ("active", Boolean Bool.true),
    ]

    decode values testInput
    == Ok {
        name: "Pencil",
        stock: 3000,
        active: Bool.true,
    }

expect
    values = Dict.fromList [
        ("name", Int 1),
        ("stock", Int 3000),
        ("active", Boolean Bool.true),
    ]

    decode values testInput
    == Err {
        path: ["name"],
        problem: InvalidValue String (Int 1),
    }

expect
    values = Dict.fromList [
        ("name", Int 1),
        ("active", Boolean Bool.true),
    ]

    decode values testInput == Err { path: ["stock"], problem: Missing }

expect
    values = Dict.fromList [
        ("name", Int 1),
        ("stock", Null),
        ("active", Boolean Bool.true),
    ]

    decode values testInput == Err { path: ["stock"], problem: NullValue }

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
        path: ["stock"],
        problem: InvalidValue Int (String "123"),
    }

# Test object

testObject =
    object "Product" {
        name: <- required "name" string,
        stock: <- optional "stock" int,
    }
    |> toType

testInputWithObject =
    const {
        product: <- required "product" testObject,
    }

expect
    values = Dict.fromList [
        (
            "product",
            Object [
                ("name", String "Pencil"),
                ("stock", Int 3000),
            ],
        ),
    ]

    decode values testInputWithObject
    == Ok {
        product: {
            name: "Pencil",
            stock: Ok 3000,
        },
    }

expect
    values = Dict.fromList [
        (
            "product",
            Object [
                ("name", String "Pencil"),
                ("stock", String "im a int sir, i swear"),
            ],
        ),
    ]

    decode values testInputWithObject
    == Err {
        path: ["product", "stock"],
        problem: InvalidValue Int (String "im a int sir, i swear"),
    }

doubleNestedTest =
    b =
        object "B" {
            c: <- required "c" int,
        }
        |> toType

    a =
        object "A" {
            b: <- required "b" b,
        }
        |> toType

    const {
        a: <- required "a" a,
    }

expect
    values = Dict.fromList [("a", Object [("b", Object [("c", Int 123)])])]

    decode values doubleNestedTest == Ok { a: { b: { c: 123 } } }

expect
    values = Dict.fromList [("a", Object [("b", Object [("c", Null)])])]

    decode values doubleNestedTest == Err { path: ["a", "b", "c"], problem: NullValue }

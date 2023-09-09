interface Gql.Enum
    exposes [
        Enum,
        Case,
        new,
        case,
        type,
    ]
    imports [
        Gql.Output.{ EnumValue, EnumMeta, Type },
        Gql.Docs.{ Describe },
    ]

Enum a := {
    meta : EnumMeta,
    value : a,
}
    implements [
        Describe {
            describe: describeEnum,
        },
    ]

describeEnum : Enum a, Str -> Enum a
describeEnum = \@Enum enum, description ->
    enumMeta = enum.meta
    @Enum { enum & meta: { enumMeta & description: Ok description } }

Case := Str

new : Str, b -> Enum b
new = \name, value ->
    @Enum {
        meta: {
            name,
            description: Err Nothing,
            values: List.withCapacity 5,
        },
        value,
    }

case : Str -> (Enum (Case -> b) -> Enum b)
case = \name ->
    \@Enum enum ->
        enumMeta = enum.meta
        @Enum {
            meta: { enumMeta &
                values: enumMeta.values |> List.append { name },
            },
            value: enum.value (@Case name),
        }

type : Enum cases, (value -> (cases -> Case)) -> Type value
type = \@Enum enum, encode -> {
    type: Enum enum.meta,
    resolve: \value, _, _ ->
        (@Case caseStr) = (encode value) enum.value
        Ok (Enum caseStr),
}


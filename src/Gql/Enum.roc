interface Gql.Enum
    exposes [
        Enum,
        Case,
        new,
        case,
        with,
        withCase,
        type,
    ]
    imports [
        Gql.Output.{
            EnumValue,
            EnumMeta,
            EnumCaseMeta,
            Type,
        },
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

new : Str, a -> Enum a
new = \name, value ->
    @Enum {
        meta: {
            name,
            description: Err Nothing,
            cases: List.withCapacity 5,
        },
        value,
    }

Case := EnumCaseMeta
    implements [
        Describe {
            describe: describeCase,
        },
    ]

describeCase : Case, Str -> Case
describeCase = \@Case meta, description ->
    @Case { meta & description: Ok description }

case : Str -> Case
case = \name ->
    @Case {
        name,
        description: Err Nothing,
    }

with : Case -> (Enum (Case -> a) -> Enum a)
with = \newCase ->
    \@Enum enum ->
        enumMeta = enum.meta

        (@Case caseMeta) = newCase

        @Enum {
            meta: { enumMeta &
                cases: enumMeta.cases |> List.append caseMeta,
            },
            value: enum.value newCase,
        }

withCase : Str -> (Enum (Case -> b) -> Enum b)
withCase = \name ->
    with (case name)

type : Enum cases, (value -> (cases -> Case)) -> Type value
type = \@Enum enum, encode -> {
    type: Enum enum.meta,
    resolve: \value, _, _ ->
        (@Case caseMeta) = (encode value) enum.value
        Ok (Enum caseMeta.name),
}


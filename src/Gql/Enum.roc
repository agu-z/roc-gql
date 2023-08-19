interface Gql.Enum
    exposes [
        Enum,
        Case,
        new,
        case,
        type,
    ]
    imports [
        Gql.Output.{ EnumValue, Type },
    ]

Enum a := {
    name : Str,
    values : List EnumValue,
    value : a,
}

Case := Str

new : Str, b -> Enum b
new = \name, value ->
    @Enum {
        name,
        values: List.withCapacity 5,
        value,
    }

case : Str -> (Enum (Case -> b) -> Enum b)
case = \name ->
    \@Enum enum ->
        @Enum {
            name: enum.name,
            values: enum.values |> List.append { name },
            value: enum.value (@Case name),
        }

type : Enum cases, (value -> (cases -> Case)) -> Type value
type = \@Enum enum, encode -> {
    type: Enum enum.name enum.values,
    resolve: \value, _, _ ->
        (@Case caseStr) = (encode value) enum.value
        Ok (Enum caseStr),
}


interface Filter
    exposes [Filter, Field, new, apply, string]
    imports [
        gql.Gql.Input.{ Input, Type },
        pg.Sql,
        pg.Sql.Types.{ PgBool, PgText },
    ]

Filter scope := scope -> List (Sql.Expr PgBool Bool)

Field scope := {
    name : Str,
    type : Type (scope -> List (Sql.Expr PgBool Bool)),
}

new : Str, List (Field scope) -> Type (Filter scope)
new = \name, fields ->
    fieldArgs =
        List.map fields \@Field field -> (field.name, field.type)

    Gql.Input.object name {
        filters: <- Gql.Input.optionalList fieldArgs,
    }
    |> Gql.Input.toType
    |> Gql.Input.map \{ filters } ->
        filterFn =
            \scope ->
                filters
                |> List.map \fieldFn -> fieldFn scope
                |> List.join

        @Filter filterFn

apply : Sql.Select a err, Filter scope, scope -> Sql.Select a err
apply = \select, @Filter toExprs, scope ->
    Sql.where select (Sql.andList (toExprs scope))

string : Str, (scope -> Sql.Expr PgText *) -> Field scope
string = \name, toExpr ->
    contains = \haystack, needle ->
        pattern =
            Sql.str "%"
            |> Sql.concat needle
            |> Sql.concat (Sql.str "%")

        Sql.like haystack pattern

    startsWith = \haystack, needle ->
        pattern =
            needle |> Sql.concat (Sql.str "%")

        Sql.like haystack pattern

    endsWith = \haystack, needle ->
        pattern =
            Sql.str "%" |> Sql.concat needle

        Sql.like haystack pattern

    makeFilter {
        name,
        typeName: "StringFilter",
        options: [
            ("eq", Sql.eq),
            ("neq", Sql.neq),
            ("contains", contains),
            ("startsWith", startsWith),
            ("endsWith", endsWith),
        ],
        type: Gql.Input.string,
        fromValue: Sql.str,
        toExpr,
    }

makeFilter = \{ name, typeName, options, type, fromValue, toExpr } ->
    optionType = \cmp -> Gql.Input.map type \value -> \expr -> cmp expr (fromValue value)

    fieldType =
        Gql.Input.object typeName {
            options: <-
                options
                |> List.map \(opName, opFn) -> (opName, optionType opFn)
                |> Gql.Input.optionalList,
        }
        |> Gql.Input.toType
        |> Gql.Input.map \args ->
            fn = \scope ->
                lhs = toExpr scope

                List.map args.options \op -> op lhs

            fn

    @Field { name, type: fieldType }

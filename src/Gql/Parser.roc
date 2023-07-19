interface Gql.Parser
    exposes []
    imports [
        ParserCore.{ Parser, map2, const, keep, skip, many, oneOrMore, sepBy1 },
        ParserStr.{ RawStr, string, parseStr, strFromRaw, codeunitSatisfies },
    ]

selectionSet : Parser RawStr (List Selection)
selectionSet =
    const \set -> set
    |> skip (string "{")
    |> skip ignored
    |> keep (sepBy1 selection ignored)
    |> skip ignored
    |> skip (string "}")

expect parseStr selectionSet "{}" |> Result.isErr
expect parseStr selectionSet "{name}" == Ok ["name"]
expect parseStr selectionSet "{ name }" == Ok ["name"]
expect parseStr selectionSet "{ name email }" == Ok ["name", "email"]
expect parseStr selectionSet "{ name\nemail }" == Ok ["name", "email"]
expect parseStr selectionSet "{ name, email }" == Ok ["name", "email"]
expect parseStr selectionSet "{\n\tname, email \n\t  phone\n}" == Ok ["name", "email", "phone"]
expect parseStr selectionSet "" |> Result.isErr
expect parseStr selectionSet "{name" |> Result.isErr
expect parseStr selectionSet "name}" |> Result.isErr

Selection : Str

selection : Parser RawStr Selection
selection = name

name : Parser RawStr Str
name =
    start, continue <- map2 nameStart nameContinue

    continue
    |> List.prepend start
    |> strFromRaw

expect parseStr name "name" == Ok "name"
expect parseStr name "productId" == Ok "productId"
expect parseStr name "User" == Ok "User"
expect parseStr name "__typename" == Ok "__typename"
expect parseStr name "users2" == Ok "users2"
expect parseStr name "2users" |> Result.isErr
expect parseStr name "product_id" == Ok "product_id"
expect parseStr name "product id" |> Result.isErr

nameStart =
    isAlpha
    |> orUnderscore
    |> codeunitSatisfies

nameContinue =
    isAlphanumeric
    |> orUnderscore
    |> codeunitSatisfies
    |> oneOrMore

orUnderscore = \option -> \code -> option code || code == '_'

isAlpha = \code ->
    (code >= 'A' && code <= 'Z') || (code >= 'a' && code <= 'z')

isNumeric = \code ->
    code >= '0' && code <= '9'

isAlphanumeric = \code ->
    isAlpha code || isNumeric code

ignored =
    # TODO: Comments
    many ignoredCodeunit

ignoredCodeunit =
    # TODO: Unicode BOM
    codeunitSatisfies \c ->
        (c == ' ')
        || (c == '\t')
        || (c == '\n')
        || (c == '\r')
        || (c == ',')


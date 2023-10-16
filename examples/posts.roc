app "posts"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        gql: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task },
        pf.Stdout,
        pf.Arg,
        gql.Gql.Schema,
        gql.Gql.Parse,
        gql.Gql.Input.{ const, required },
        gql.Gql.Enum,
        gql.Gql.Value.{ Value },
        gql.Gql.Output.{
            Object,
            string,
            int,
            listOf,
            nullable,
            ref,
            object,
            field,
            retField,
        },
        # Need to be imported to make compiler happy
        gql.Gql.Document,
    ]
    provides [main] to pf

# -- SCHEMA --

schema =
    { query }

query : Object {} Value
query =
    object "Query" [
        retField "posts" (listOf (ref post)) \_ -> postsData,
        field "post" (nullable (ref post)) {
            takes: const {
                id: <- required "id" Gql.Input.int,
            },
            resolve: \_, { id } ->
                postsData
                |> List.findFirst \p -> p.id == id
                |> Result.mapErr \NotFound -> Nothing,
        },
    ]

postsData : List Post
postsData = [
    {
        id: 1,
        title: "Hi",
        body: Ok "Testing",
        section: News,
        author: { firstName: "John", lastName: "Doe" },
    },
    {
        id: 2,
        title: "Post 2",
        body: Err Nothing,
        section: Opinion,
        author: { firstName: "Agus", lastName: "Zubiaga" },
    },
]

Post : {
    id : I32,
    title : Str,
    body : Result Str [Nothing],
    author : Author,
    section : [News, Opinion],
}

post : Object Post Value
post =
    object "Post" [
        retField "id" int .id,
        retField "title" string .title,
        retField "body" (nullable string) .body,
        retField "author" (ref author) .author,
        retField "section" postSection .section,
    ]

postSection =
    Gql.Enum.new "PostSection" {
        news: <- Gql.Enum.withCase "NEWS",
        opinion: <- Gql.Enum.withCase "OPINION",
    }
    |> Gql.Enum.type \value ->
        when value is
            News ->
                .news

            Opinion ->
                .opinion

Author : {
    firstName : Str,
    lastName : Str,
}

author : Object Author Value
author =
    object "Author" [
        retField "firstName" string .firstName,
        retField "lastName" string .lastName,
    ]

# -- PARSE AND EXECUTE --

main : Task {} I32
main =
    args <- Arg.list |> Task.await

    when args is
        [_, inputQuery] ->
            result =
                document <-
                    Gql.Parse.parseDocument inputQuery
                    |> Result.try

                Gql.Schema.execute {
                    schema,
                    document,
                    operation: First,
                    variables: Dict.empty {},
                    rootValue: {},
                    fromValue: \value -> value,
                }

            when result is
                Ok res ->
                    Stdout.line (Gql.Value.toJson (Object [("data", Object res)]))

                Err (ParsingFailure fail) ->
                    Stdout.line "Parsing failure: \(fail)"

                Err (ParsingIncomplete fail) ->
                    Stdout.line "Parsing incomplete: \(fail)"

                Err _ ->
                    Stdout.line "ERR"

        _ ->
            Task.err 1

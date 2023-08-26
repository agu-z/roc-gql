interface ExampleSchema
    exposes [schema]
    imports [
        Gql.Output.{
            Object,
            string,
            int,
            listOf,
            nullable,
            ref,
            object,
            field,
        },
        Gql.Input.{ const, required },
        Gql.Enum,
    ]

schema =
    { query }

query : Object {}
query =
    object "Query" [
        field "posts" (listOf (ref post)) (ret \_ -> postsData),
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

post : Object Post
post =
    object "Post" [
        field "id" int (ret .id),
        field "title" string (ret .title),
        field "body" (nullable string) (ret .body),
        field "author" (ref author) (ret .author),
        field "section" postSection (ret .section),
    ]

postSection =
    Gql.Enum.new "PostSection" {
        news: <- Gql.Enum.case "NEWS",
        opinion: <- Gql.Enum.case "OPINION",
    }
    |> Gql.Enum.type \value ->
        when value is
            News ->
                .news

            Opinion ->
                .news

Author : {
    firstName : Str,
    lastName : Str,
}

author : Object Author
author =
    object "Author" [
        field "firstName" string (ret .firstName),
        field "lastName" string (ret .lastName),
    ]

ret = \fn -> { takes: const {}, resolve: \v, _ -> fn v }

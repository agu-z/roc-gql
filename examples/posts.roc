app "posts"
    packages {
        pf: "https://github.com/roc-lang/basic-webserver/releases/download/0.2.0/J6CiEdkMp41qNdq-9L3HGoF2cFkafFlArvfU1RtR4rY.tar.br",
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.5.0/jEPD_1ZLFiFrBeYKiKvHSisU-E3LZJeenfa9nvqJGeE.tar.br",
        gql: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task },
        pf.Http.{ Request, Response },
        pf.Stdout,
        json.Core,
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

handleReq : Request -> Task Response _
handleReq = \req ->
    when req.body is
        EmptyBody ->
            Task.ok {
                status: 400,
                headers: [],
                body: "Expected JSON object with GraphQL query" |> Str.toUtf8,
            }

        Body { body } ->
            data <-
                body
                |> Decode.fromBytes Core.json
                |> Result.mapErr JsonErr
                |> Result.try \json ->
                    Gql.Parse.parseDocument json.query
                    |> Result.mapErr ParseErr
                |> Result.try \document ->
                    Gql.Schema.execute {
                        schema,
                        document,
                        operation: First,
                        variables: Dict.empty {},
                        rootValue: {},
                        fromValue: \value -> value,
                    }
                    |> Result.mapErr ExecuteErr
                |> Task.fromResult
                |> Task.await

            Task.ok {
                status: 200,
                headers: [
                    {
                        name: "Content-Type",
                        value: "application/json" |> Str.toUtf8,
                    },
                ],
                body: Object [("data", Object data)]
                |> Gql.Value.toJson
                |> Str.toUtf8,
            }

main : Request -> Task Response []
main = \req ->
    result <- Task.attempt (handleReq req)

    when result is
        Ok ok ->
            Task.ok ok

        Err (ExecuteErr err) ->
            err
            |> Gql.Schema.executeErrToStr
            |> respondWithError 400

        Err (SelectionErr err) ->
            err
            |> Gql.Output.resolveErrToStr
            |> respondWithError 400

        Err _ ->
            respondWithError "Something went wrong" 500

respondWithError : Str, U16 -> Task Response []
respondWithError = \msg, status ->
    Task.ok {
        status,
        headers: [
            {
                name: "Content-Type",
                value: "application/json" |> Str.toUtf8,
            },
        ],
        body: Object [("error", String msg)]
        |> Gql.Value.toJson
        |> Str.toUtf8,
    }

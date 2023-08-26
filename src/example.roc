app "example"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Task.{ Task },
        pf.Stdout,
        pf.Arg,
        Gql.Schema,
        Gql.Parse,
        Gql.Value,
        ExampleSchema,
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await

    when args is
        [_, query] ->
            result =
                document <-
                    Gql.Parse.parseDocument query
                    |> Result.try

                Gql.Schema.execute {
                    schema: ExampleSchema.schema,
                    document,
                    operation: First,
                    variables: Dict.empty {},
                    rootValue: {},
                }

            when result is
                Ok res ->
                    Stdout.line (Gql.Value.toJson (Object [("data", res)]))

                Err (ParsingFailure fail) ->
                    Stdout.line "Parsing failure: \(fail)"

                Err (ParsingIncomplete fail) ->
                    Stdout.line "Parsing incomplete: \(fail)"

                Err _ ->
                    Stdout.line "ERR"

        _ ->
            Task.err 1

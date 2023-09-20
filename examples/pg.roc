app "pg"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.3.2/tE4xS_zLdmmxmHwHih9kHWQ7fsXtJr7W7h3425-eZFk.tar.br",
        pg: "../../roc-pg/src/main.roc",
        gql: "../src/main.roc",
    }
    imports [
        pf.Task.{ Task },
        pf.Stdout,
        pf.Stderr,
        pf.Arg,
        pf.Process,
        pg.Pg.Client,
        pg.Sql.{ Selection },
        gql.Gql.Schema,
        gql.Gql.Parse,
        gql.Gql.Value.{ Value },
        gql.Gql.Output.{ Object, object, string, int, field },
        gql.Gql.Input.{ const },
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        gql.Gql.Document,
        gql.Gql.Enum,
        pg.Pg.Result,
        pg.Pg.Cmd,
    ]
    provides [main] to pf

# -- SCHEMA --

schema =
    { query }

query : Object {} (Selection Value)
query =
    object "Query" [
        field "one" (selExpr int) (ret \_ -> Sql.i32 42),
        field "two" (selExpr string) (ret \_ -> Sql.str "sup"),
    ]

selExpr = \t -> {
    type: t.type,
    resolve: \a, b, c ->
        Sql.into \val ->
            when t.resolve val b c is
                Ok result ->
                    result

                _ ->
                    crash "fail"
        |> (Sql.column a)
        |> Ok,
}

ret = \fn -> { takes: const {}, resolve: \v, _ -> fn v }

# -- PARSE AND EXECUTE --

task : Task {} _
task =
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
                    fromValue: Sql.into,
                }

            when result is
                Ok selections ->
                    client <- Pg.Client.withConnect {
                            host: "localhost",
                            port: 5432,
                            user: "postgres",
                            database: "postgres",
                        }

                    pgRes <-
                        selections
                        |> List.map \(name, sel) -> Sql.map sel \value -> (name, value)
                        |> Sql.selectionList
                        |> Sql.querySelection
                        |> Pg.Client.command client
                        |> Task.await

                    Stdout.line (Gql.Value.toJson (Object [("data", Object pgRes)]))

                Err (ParsingFailure fail) ->
                    Stdout.line "Parsing failure: \(fail)"

                Err (ParsingIncomplete fail) ->
                    Stdout.line "Parsing incomplete: \(fail)"

                Err err ->
                    dbg
                        err

                    Stdout.line "ERR"

        _ ->
            Process.exit 1

main =
    result <- Task.attempt task

    when result is
        Ok _ ->
            Process.exit 0

        Err (TcpPerformErr (PgErr err)) ->
            _ <- Stderr.line (Pg.Client.errorToStr err) |> Task.await
            Process.exit 2

        Err err ->
            dbg
                err

            _ <- Stderr.line "Something went wrong" |> Task.await
            Process.exit 1

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
        gql.Gql.Output.{ Object, object, string, field, retField, ResolveErr, Type },
        gql.Gql.Input.{ const, required },
        VideoRental,
        Filter,
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

query =
    object "Query" [
        field "films" (listRef film) {
            takes: const {
                limit: <- required "limit" Gql.Input.int,
                filter: <- required "filter" filmFilter,
            },
            resolve: \{}, args ->
                films <- Sql.from VideoRental.film

                Sql.select films
                |> Sql.limit (Num.toNat args.limit)
                |> Filter.apply args.filter films,
        },
    ]

filmFilter =
    Filter.new "FilmFilter" [
        Filter.string "title" .title,
        Filter.int "releaseYear" .releaseYear,
    ]

film =
    object "Film" [
        selField "title" string .title,
        selField "description" string .description,
        field "actor" (ref actor) {
            takes: const {},
            resolve: \films, {} ->
                actors <- Sql.from VideoRental.actor
                filmActors <- Sql.join VideoRental.filmActor (Sql.on .actorId actors.actorId)

                Sql.select actors
                |> Sql.where (Sql.eq filmActors.filmId films.filmId),
        },
    ]

actor =
    object "Actor" [
        selField "firstName" string .firstName,
        selField "lastName" string .lastName,
    ]

# HELPERS

selField = \name, type, resolve ->
    retField name (selExpr type) resolve

selExpr = \t -> {
    type: t.type,
    resolve: \expr, selection, opCtx ->
        Sql.into \val ->
            when t.resolve val selection opCtx is
                Ok result ->
                    result

                Err _ ->
                    crash "unreachable"
        |> (Sql.column expr)
        |> Ok,
}

ref : Object scope (Sql.Selection Value ResolveErr) -> Type (Sql.Query scope ResolveErr) (Sql.Selection Value ResolveErr)
ref = \obj -> {
    type: Ref (Gql.Output.objectMeta obj),
    resolve: \queryScope, selection, opCtx ->
        querySelection =
            Sql.tryMapQuery queryScope \scope ->
                selections <-
                    Gql.Output.resolveObject obj (\str -> Sql.into (String str)) scope selection opCtx
                    |> Result.map

                selections
                |> List.map \(name, sel) -> Sql.map sel \value -> (name, value)
                |> Sql.selectionList
                |> Sql.map Object

        Sql.into \x -> x
        |> (Sql.row querySelection)
        |> Ok,
}

listRef : Object scope (Sql.Selection Value ResolveErr) -> Type (Sql.Query scope ResolveErr) (Sql.Selection Value ResolveErr)
listRef = \obj -> {
    type: List (Ref (Gql.Output.objectMeta obj)),
    resolve: \queryScope, selection, opCtx ->
        querySelection =
            Sql.tryMapQuery queryScope \scope ->
                selections <-
                    Gql.Output.resolveObject obj (\str -> Sql.into (String str)) scope selection opCtx
                    |> Result.map

                selections
                |> List.map \(name, sel) -> Sql.map sel \value -> (name, value)
                |> Sql.selectionList
                |> Sql.map Object

        Sql.into List
        |> (Sql.rowArray querySelection)
        |> Ok,
}

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
                            database: "pagalia",
                        }

                    pgCmd <-
                        selections
                        |> List.map \(name, sel) -> Sql.map sel \value -> (name, value)
                        |> Sql.selectionList
                        |> Sql.querySelection
                        |> Task.fromResult
                        |> Task.mapFail ResolveErr
                        |> Task.await

                    _ <- Stdout.line (Pg.Cmd.inspect pgCmd) |> Task.await

                    pgRes <-
                        pgCmd
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

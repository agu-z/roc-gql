app "pg"
    packages {
        pf: "https://github.com/roc-lang/basic-webserver/releases/download/0.2.0/J6CiEdkMp41qNdq-9L3HGoF2cFkafFlArvfU1RtR4rY.tar.br",
        json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.5.0/jEPD_1ZLFiFrBeYKiKvHSisU-E3LZJeenfa9nvqJGeE.tar.br",
        pg: "../../../roc-pg/src/main.roc",
        gql: "../../src/main.roc",
    }
    imports [
        pf.Task.{ Task, await },
        pf.Stdout,
        pf.Http.{ Request, Response },
        pg.Pg.Client,
        pg.Sql.{ Selection },
        json.Core,
        gql.Gql.Schema,
        gql.Gql.Parse,
        gql.Gql.Value.{ Value },
        gql.Gql.Output.{ Object, object, string, int, field, retField, ResolveErr, Type },
        gql.Gql.Input.{ const, required, optional },
        Public,
        Filter,
        # Unused but required because of: https://github.com/roc-lang/roc/issues/5477
        gql.Gql.Document,
        gql.Gql.Enum,
        pg.Pg.Result,
        pg.Pg.Cmd,
        pf.Tcp,
    ]
    provides [main] to pf

# -- SCHEMA --

schema =
    { query }

query = object "Query" [
    field "customers" (listRef customer) {
        takes: const {},
        resolve: \{}, {} ->
            customers <- Sql.from Public.customers

            Sql.select customers,
    },
    field "customer" (ref customer) {
        takes: const {
            id: <- required "id" Gql.Input.int,
        },
        resolve: \{}, args ->
            customers <- Sql.from Public.customers

            Sql.select customers
            |> Sql.where (Sql.eq customers.id (Sql.i32 args.id)),
    },
    field "order" (ref order) {
        takes: const {
            id: <- required "id" Gql.Input.int,
        },
        resolve: \{}, args ->
            orders <- Sql.from Public.orders

            Sql.select orders
            |> Sql.where (Sql.eq orders.id (Sql.i32 args.id)),
    },
    field "products" (listRef product) {
        takes: const {
            filter: <- optional "filter" productFilter,
        },
        resolve: \{}, args ->
            products <- Sql.from Public.products

            Sql.select products
            |> Filter.apply args.filter products,
    },
]

productFilter =
    Filter.new "ProductFilter" [
        Filter.string "name" .name,
        Filter.int "modelYear" .modelYear,
    ]

product = \{} ->
    object "Product" [
        selField "name" string .name,
        selField "modelYear" int .modelYear,
    ]

customer = \{} ->
    object "Customer" [
        selField "id" int .id,
        selField "firstName" string .firstName,
        selField "lastName" string .lastName,
        selField "fullName" string \c ->
            c.firstName
            |> Sql.concat (Sql.str " ")
            |> Sql.concat c.lastName,
        selField "email" string .email,
        field "orders" (listRef order) {
            takes: const {},
            resolve: \customers, {} ->
                orders <- Sql.from Public.orders

                Sql.select orders
                |> Sql.where (Sql.eq orders.customerId customers.id),
        },
    ]

order = \{} ->
    object "Order" [
        selField "id" int .id,
        selField "createdAt" string .createdAt,
        field "products" (listRef orderProduct) {
            takes: const {},
            resolve: \orders, {} ->
                orderProducts <- Sql.from Public.orderProducts

                Sql.select orderProducts
                |> Sql.where (Sql.eq orderProducts.orderId orders.id),
        },
    ]

orderProduct = \{} ->
    object "OrderProduct" [
        selField "quantity" int .quantity,
        field "product" (ref product) {
            takes: const {},
            resolve: \orderProducts, {} ->
                products <- Sql.from Public.products

                Sql.select products
                |> Sql.where (Sql.eq products.id orderProducts.productId),
        },
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

ref : ({} -> Object scope (Sql.Selection Value ResolveErr)) -> Type (Sql.Query scope ResolveErr) (Sql.Selection Value ResolveErr)
ref = \toObj ->
    obj = toObj {}

    {
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

listRef : ({} -> Object scope (Sql.Selection Value ResolveErr)) -> Type (Sql.Query scope ResolveErr) (Sql.Selection Value ResolveErr)
listRef = \toObj ->
    obj = toObj {}
    {
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
            selections <-
                body
                |> Decode.fromBytes Core.json
                |> Result.mapErr JsonErr
                |> Result.try \json ->
                    json.query
                    |> Str.trim
                    |> Gql.Parse.parseDocument
                    |> Result.mapErr ParseErr
                |> Result.try \document ->
                    Gql.Schema.execute {
                        schema,
                        document,
                        operation: First,
                        variables: Dict.empty {},
                        rootValue: {},
                        fromValue: Sql.into,
                    }
                    |> Result.mapErr ExecuteErr
                |> Task.fromResult
                |> await

            pgCmd <-
                selections
                |> List.map \(name, sel) -> Sql.map sel \value -> (name, value)
                |> Sql.selectionList
                |> Sql.querySelection
                |> Task.fromResult
                |> Task.mapErr SelectionErr
                |> Task.await

            client <- Pg.Client.withConnect {
                    host: "localhost",
                    port: 5432,
                    user: "postgres",
                    database: "roc_pg_example",
                }

            _ <- Stdout.line (Pg.Cmd.inspect pgCmd) |> Task.await

            pgRes <-
                pgCmd
                |> Pg.Client.command client
                |> Task.await

            Task.ok {
                status: 200,
                headers: [
                    {
                        name: "Content-Type",
                        value: "application/json" |> Str.toUtf8,
                    },
                ],
                body: Object [("data", Object pgRes)]
                |> Gql.Value.toJson
                |> Str.toUtf8,
            }

main : Request -> Task Response []
main = \req ->
    result <- Task.attempt (handleReq req)

    when result is
        Ok ok ->
            Task.ok ok

        Err (ParseErr err) ->
            err
            |> Gql.Parse.errToStr
            |> respondWithError 400

        Err (JsonErr _) ->
            "Failed to parse body JSON"
            |> respondWithError 400

        Err (ExecuteErr err) ->
            err
            |> Gql.Schema.executeErrToStr
            |> respondWithError 400

        Err (SelectionErr err) ->
            err
            |> Gql.Output.resolveErrToStr
            |> respondWithError 400

        Err (TcpPerformErr (PgErr err)) ->
            err
            |> Pg.Client.errorToStr
            |> respondWithError 500

        Err (TcpPerformErr _) ->
            respondWithError "Something went wrong while performing the database query" 500

        Err (TcpConnectErr err) ->
            respondWithError "Failed to connect to database: \(Tcp.connectErrToStr err)" 500

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

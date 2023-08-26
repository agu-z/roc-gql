# roc-gql

[WIP] GraphQL in Roc experiment. Far from spec-compliant.

## Example

1. Build the example app:

```shell
$ roc build src/example.roc
```

2. Run the example server: 

```shell
$ deno run --allow-net --allow-run example-server.ts
```

> Using deno in this example only because it's an easy way to spin up an HTTP server. [Parsing](https://github.com/agu-z/roc-gql/blob/main/src/Gql/Parse.roc) and [executing](https://github.com/agu-z/roc-gql/blob/e79480e2092a4a5167ea39ddeae3b9e8c24397fb/src/Gql/Schema.roc#L45) the query is all done in Roc.

3. Point your prefered GraphQL playground app (such as [Altair](https://altairgraphql.dev)) to [http://localhost:8080](http://localhost:8080) and run a query!

### Example query

```graphql
query {
  posts {
    ...PostBasics
  }

  postNumberOne: post(id: 1) {
    ...PostBasics
    body
    author {
      firstName
      lastName
    }
  }
}

fragment PostBasics on Post {
  id
  title
  section
}
```
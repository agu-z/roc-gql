# roc-gql

GraphQL in Roc experiment

## ⚠️ Archived

I’ve always appreciated the type-safety provided by GraphQL, but its query flexibility can be a double-edged sword. Over my 5+ years of professional experience with GraphQL, I have seen backends balloon in complexity as they try to get data fetching and authorization right. It's just really hard to reason about this when the client could request anything.

If I were starting a new app, I'd avoid SPAs altogether and go with something simpler like [htmx](https://htmx.org) and a REST API. I'm also interested in exploring [gRPC](https://grpc.io) and what a Roc server could look like.

Given these considerations, I have decided to stop pursuing this idea.

## Example

1. Run the server: 

```shell
$ roc examples/posts.roc
```

2. Point your prefered GraphQL playground app (such as [Altair](https://altairgraphql.dev)) to [http://localhost:8000](http://localhost:8000) and run a query!

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

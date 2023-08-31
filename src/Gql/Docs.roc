interface Gql.Docs
    exposes [
        Describe,
        describe,
        Deprecate,
        deprecate,
    ] imports []

Describe implements
    describe : v, Str -> v where v implements Describe

Deprecate implements
    deprecate : v, Str -> v where v implements Deprecate

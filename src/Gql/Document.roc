interface Gql.Document
    exposes [
        Document,
        Definition,
        OperationType,
        VariableDefinition,
        Type,
        NamedOrListType,
        Selection,
        Argument,
        Value,
        findOperation,
        canSelection,
        CanSelection,
    ]
    imports []

Document : List Definition

Definition : [
    Operation Operation,
    Fragment Fragment,
]

Fragment : {
    name : Str,
    typeName : Str,
    # TODO: Directives
    selectionSet : List Selection,
}

Operation : {
    type : OperationType,
    name : Result Str [Nothing],
    variables : List VariableDefinition,
    # TODO: Directives
    selectionSet : List Selection,
}

OperationType : [Query, Mutation, Subscription]

VariableDefinition : {
    name : Str,
    type : Type,
    default : Result Value [Nothing],
    directives : List Directive,
}

Directive : (Str, List Argument)

Type : [
    Nullable NamedOrListType,
    NonNull NamedOrListType,
]

NamedOrListType : [
    Named Str,
    ListT Type,
]

Selection : [
    Field
        {
            field : Str,
            alias : Result Str [Nothing],
            arguments : List Argument,
            # TODO: Directives
            selectionSet : List Selection,
        },
    FragmentSpread Str,
    InlineFragment
        {
            typeName : Result Str [Nothing],
            # TODO: Directives
            selectionSet : List Selection,
        },
]

Argument : (Str, Value)

Value : [
    Var Str,
    Int I32,
    String Str,
    Boolean Bool,
    Null,
    Enum Str,
    List (List Value),
    Object (List (Str, Value)),
    # TODO:
    # FloatValue
]

findOperation : Document, [First, ByName Str] -> Result Operation [OperationNotFound]
findOperation = \doc, rule ->
    state, def <- List.walkUntil doc (Err OperationNotFound)

    when def is
        Operation op ->
            when (op.name, rule) is
                (_, First) ->
                    Break (Ok op)

                (Ok thisName, ByName wantedName) if thisName == wantedName ->
                    Break (Ok op)

                _ ->
                    Continue state

        _ ->
            Continue state

expect findOperation [] First == Err OperationNotFound
expect findOperation [Operation testOp] (ByName "GetUser") == Ok testOp
expect findOperation [Operation testOp] First == Ok testOp
expect findOperation [Operation testOp] (ByName "getUser") == Err OperationNotFound

testOp = { type: Query, name: Ok "GetUser", variables: [], selectionSet: [] }

findFragment : Document, Str -> Result Fragment [FragmentNotFound]
findFragment = \doc, name ->
    # TODO: Use dict?
    state, def <- List.walkUntil doc (Err FragmentNotFound)

    when def is
        Fragment fragment ->
            if fragment.name == name then
                Break (Ok fragment)
            else
                Continue state

        _ ->
            Continue state

expect findFragment [] "PostDetails" == Err FragmentNotFound
expect findFragment [Fragment testFragment] "PostDetails" == Ok testFragment
expect findFragment [Fragment testFragment] "Comment" == Err FragmentNotFound

testFragment = { name: "PostDetails", typeName: "Post", selectionSet: [] }

CanSelection : [
    CanField
        {
            field : Str,
            alias : Result Str [Nothing],
            arguments : List Argument,
            # TODO: Directives
            selectionSet : List CanSelection,
        },
]

canSelection : Selection, Document -> Result (List CanSelection) [FragmentNotFound Str, RecursiveFragment Str]
canSelection = \sel, doc ->
    canSelectionHelp sel doc (Set.empty {})

canSelectionHelp = \sel, doc, seenFragments ->
    when sel is
        Field field ->
            selections <- field.selectionSet
                |> List.mapTry \subSel -> canSelection subSel doc
                |> Result.map

            [
                CanField {
                    field: field.field,
                    alias: field.alias,
                    arguments: field.arguments,
                    selectionSet: List.join selections,
                },
            ]

        FragmentSpread name ->
            if Set.contains seenFragments name then
                Err (RecursiveFragment name)
            else
                fragment <-
                    findFragment doc name
                    |> Result.mapErr \FragmentNotFound -> FragmentNotFound name
                    |> Result.try

                newSeenFragments =
                    seenFragments |> Set.insert name

                fragment.selectionSet
                |> List.mapTry \subSel -> canSelectionHelp subSel doc newSeenFragments
                |> Result.map List.join

        _ ->
            crash "todo"

expect
    doc = [
        Fragment {
            name: "Post",
            typeName: "Post",
            selectionSet: [
                Field { field: "body", alias: Err Nothing, arguments: [], selectionSet: [] },
                Field {
                    field: "author",
                    alias: Err Nothing,
                    arguments: [],
                    selectionSet: [
                        FragmentSpread "User",
                    ],
                },
            ],
        },
        Fragment {
            name: "User",
            typeName: "User",
            selectionSet: [
                Field { field: "name", alias: Err Nothing, arguments: [], selectionSet: [] },
            ],
        },
    ]

    sel = Field {
        field: "posts",
        alias: Err Nothing,
        arguments: [],
        selectionSet: [
            Field { field: "title", alias: Err Nothing, arguments: [], selectionSet: [] },
            FragmentSpread "Post",
        ],
    }

    expected = [
        CanField {
            field: "posts",
            alias: Err Nothing,
            arguments: [],
            selectionSet: [
                CanField { field: "title", alias: Err Nothing, arguments: [], selectionSet: [] },
                CanField { field: "body", alias: Err Nothing, arguments: [], selectionSet: [] },
                CanField {
                    field: "author",
                    alias: Err Nothing,
                    arguments: [],
                    selectionSet: [
                        CanField { field: "name", alias: Err Nothing, arguments: [], selectionSet: [] },
                    ],
                },
            ],
        },
    ]

    canSelection sel doc == Ok expected

expect
    doc = [
        Fragment {
            name: "Post",
            typeName: "Post",
            selectionSet: [FragmentSpread "Post"],
        },
    ]

    sel = Field {
        field: "posts",
        alias: Err Nothing,
        arguments: [],
        selectionSet: [FragmentSpread "Post"],
    }

    canSelection sel doc == Err (RecursiveFragment "Post")

# TODO: Figure out why this stack overflows:
# expect
#    doc = [
#        Fragment {
#            name: "User",
#            typeName: "User",
#            selectionSet: [
#                Field { field: "posts", alias: Err Nothing, arguments: [], selectionSet: [FragmentSpread "Post"] },
#            ],
#        },
#        Fragment {
#            name: "Post",
#            typeName: "Post",
#            selectionSet: [
#                Field { field: "author", alias: Err Nothing, arguments: [], selectionSet: [FragmentSpread "User"] },
#            ],
#        },
#    ]
#
#    sel = Field {
#        field: "posts",
#        alias: Err Nothing,
#        arguments: [],
#        selectionSet: [FragmentSpread "Post"],
#    }
#
#    canSelection sel doc == Err (RecursiveFragment "Post")

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
    ]
    imports []

Document : List Definition

Definition : [
    Operation Operation,
    Fragment
        {
            name : Str,
            typeName : Str,
            # TODO: Directives
            selectionSet : List Selection,
        },
]

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
    # TODO: Directives
}

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
    Variable Str,
    IntValue I32,
    StringValue Str,
    BooleanValue Bool,
    NullValue,
    EnumValue Str,
    ListValue (List Value),
    ObjectValue (List (Str, Value)),
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

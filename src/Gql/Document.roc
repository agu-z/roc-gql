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
    ]
    imports []

Document : List Definition

Definition : [
    Operation
        {
            type : OperationType,
            name : Result Str [Nothing],
            variables : List VariableDefinition,
            # TODO: Directives
            selectionSet : List Selection,
        },
    Fragment
        {
            name : Str,
            typeName : Str,
            # TODO: Directives
            selectionSet : List Selection,
        },
]

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


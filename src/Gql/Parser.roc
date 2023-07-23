interface Gql.Parser
    exposes [selection]
    imports [
        ParserCore.{
            Parser,
            map,
            map2,
            between,
            fail,
            const,
            keep,
            skip,
            many,
            oneOrMore,
            sepBy1,
            sepBy,
            maybe,
            andThen,
        },
        ParserStr.{
            RawStr,
            parseStr,
            strFromRaw,
            codeunit,
            codeunitSatisfies,
            oneOf,
            string,
        },
    ]

# https://spec.graphql.org/October2021

# Document

Document : List Definition

document : Parser RawStr Document
document =
    definition
    |> sepBy1 ignored

expect
    parseStr
        document
        """
        query GetUser {
            me {
                id
                name
            }
        }

        query {
            posts {
                id
                title
            }
        }
        """
    == Ok [
        Operation {
            type: Query,
            name: Ok "GetUser",
            selectionSet: [
                testField "me"
                |> withSelection [
                    testField "id",
                    testField "name",
                ],
            ],
        },
        Operation {
            type: Query,
            name: Err Nothing,
            selectionSet: [
                testField "posts"
                |> withSelection [
                    testField "id",
                    testField "title",
                ],
            ],
        },
    ]

# Definition

Definition : [
    Operation
        {
            type : OperationType,
            name : Result Str [Nothing],
            # TODO: Variable definitions
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

definition : Parser RawStr Definition
definition =
    oneOf [
        operationDefinition,
        fragmentDefinition,
    ]

# Operation Definition

operationDefinition : Parser RawStr Definition
operationDefinition =
    const \typ -> \nam -> \ss -> Operation { type: typ, name: nam, selectionSet: ss }
    |> keep (maybe opType |> withDefault Query)
    |> skip ignored
    |> keep (maybe name)
    |> skip ignored
    |> keep selectionSet

OperationType : [Query, Mutation, Subscription]

opType : Parser RawStr OperationType
opType =
    oneOf [
        string "query" |> map \_ -> Query,
        string "mutation" |> map \_ -> Mutation,
        string "subscription" |> map \_ -> Subscription,
    ]

expect
    parseStr operationDefinition "query { user { id } }"
    == Ok
        (
            Operation {
                type: Query,
                name: Err Nothing,
                selectionSet: [testField "user" |> withSelection [testField "id"]],
            }
        )
expect
    parseStr operationDefinition "query GetUser { user { id } }"
    == Ok
        (
            Operation {
                type: Query,
                name: Ok "GetUser",
                selectionSet: [testField "user" |> withSelection [testField "id"]],
            }
        )
expect
    parseStr operationDefinition "mutation LogOut { logOut { success } }"
    == Ok
        (
            Operation {
                type: Mutation,
                name: Ok "LogOut",
                selectionSet: [testField "logOut" |> withSelection [testField "success"]],
            }
        )
expect
    parseStr operationDefinition "subscription { messages { id body } }"
    == Ok
        (
            Operation {
                type: Subscription,
                name: Err Nothing,
                selectionSet: [testField "messages" |> withSelection [testField "id", testField "body"]],
            }
        )
expect
    parseStr operationDefinition "{ user { id } }"
    == Ok
        (
            Operation {
                type: Query,
                name: Err Nothing,
                selectionSet: [testField "user" |> withSelection [testField "id"]],
            }
        )

# Type

Type : [
    Nullable NamedOrListType,
    NonNull NamedOrListType,
]

NamedOrListType : [
    Named Str,
    ListT Type,
]

type : Parser RawStr Type
type =
    oneOf [
        nonNullType,
        namedOrListType |> map Nullable,
    ]

expect parseStr type "User" == Ok (Named "User" |> Nullable)
expect parseStr type "User!" == Ok (Named "User" |> NonNull)
expect parseStr type "[User]" == Ok (Named "User" |> Nullable |> ListT |> Nullable)
expect parseStr type "[User!]" == Ok (Named "User" |> NonNull |> ListT |> Nullable)
expect parseStr type "[User!]!" == Ok (Named "User" |> NonNull |> ListT |> NonNull)
expect parseStr type "[[User]!]!" == Ok (Named "User" |> Nullable |> ListT |> NonNull |> ListT |> NonNull)

nonNullType : Parser RawStr Type
nonNullType =
    const NonNull
    |> keep namedOrListType
    |> skip ignored
    |> skip (codeunit '!')

namedOrListType : Parser RawStr NamedOrListType
namedOrListType =
    oneOf [
        namedType,
        listType,
    ]

namedType : Parser RawStr NamedOrListType
namedType =
    name |> map Named

listType : Parser RawStr NamedOrListType
listType =
    const ListT
    |> skip (codeunit '[')
    |> skip ignored
    |> keep recursiveType
    |> skip ignored
    |> skip (codeunit ']')

# Fragment Definition

fragmentDefinition : Parser RawStr Definition
fragmentDefinition =
    const \fname -> \typeName -> \ss -> Fragment { name: fname, typeName, selectionSet: ss }
    |> skip (string "fragment")
    |> skip ignored
    |> keep fragmentName
    |> skip ignored
    |> skip (string "on")
    |> skip ignored
    |> keep name
    |> skip ignored
    |> keep selectionSet

expect
    parsed = parseStr fragmentDefinition "fragment UserDetails on User { id name posts { id, title } }"
    parsed
    == Ok
        (
            Fragment {
                name: "UserDetails",
                typeName: "User",
                selectionSet: [
                    testField "id",
                    testField "name",
                    testField "posts"
                    |> withSelection [
                        testField "id",
                        testField "title",
                    ],
                ],
            }
        )

fragmentName : Parser RawStr Str
fragmentName =
    fname <- name |> andThen

    if fname == "on" then
        fail "Fragment name must not be 'on'"
    else
        const fname

expect parseStr fragmentName "UserDetails" == Ok "UserDetails"
expect parseStr fragmentName "on" |> Result.isErr

# Selection

Selection : [
    Field
        {
            field : Str,
            alias : Result Str [Nothing],
            arguments : List Argument,
            # TODO: Arguments
            # TODO: Directives
            selectionSet : List Selection,
        },
    # TODO: Fragments
]

selection : Parser RawStr Selection
selection =
    field

# Selection Set

selectionSet : Parser RawStr (List Selection)
selectionSet =
    const identity
    |> skip (codeunit '{')
    |> skip ignored
    |> keep (selection |> sepBy1 ignored)
    |> skip ignored
    |> skip (codeunit '}')

expect parseStr selectionSet "{}" |> Result.isErr
expect parseStr selectionSet "{name}" == Ok [testField "name"]
expect parseStr selectionSet "{ name }" == Ok [testField "name"]
expect parseStr selectionSet "{ name email }" == Ok [testField "name", testField "email"]
expect parseStr selectionSet "{ name\nemail }" == Ok [testField "name", testField "email"]
expect parseStr selectionSet "{ name, email }" == Ok [testField "name", testField "email"]
expect
    parseStr
        selectionSet
        """
        {
            fullName: name, 
              email
            phone
        }
        """
    == Ok [
        testField "name" |> withAlias "fullName",
        testField "email",
        testField "phone",
    ]
expect parseStr selectionSet "" |> Result.isErr
expect parseStr selectionSet "{name" |> Result.isErr
expect parseStr selectionSet "name}" |> Result.isErr

# Field

field : Parser RawStr Selection
field =
    const \left -> \right -> \args -> \ss -> mkField left right args ss
    |> keep name
    |> skip ignored
    |> keep (maybe colonAndFieldName)
    |> skip ignored
    |> keep (maybe arguments |> withDefault [])
    |> skip ignored
    |> keep (maybe recursiveSelectionSet |> withDefault [])

mkField = \left, right, args, ss ->
    when right is
        Ok f ->
            Field {
                field: f,
                alias: Ok left,
                arguments: args,
                selectionSet: ss,
            }

        Err Nothing ->
            Field {
                field: left,
                alias: Err Nothing,
                arguments: args,
                selectionSet: ss,
            }

colonAndFieldName =
    const identity
    |> skip (codeunit ':')
    |> skip ignored
    |> keep name

expect parseStr field "name" == Ok (testField "name")
expect parseStr field "fullName:name" == Ok (testField "name" |> withAlias "fullName")
expect parseStr field "fullName: name" == Ok (testField "name" |> withAlias "fullName")
expect parseStr field "fullName : name" == Ok (testField "name" |> withAlias "fullName")
expect parseStr field "post(id: 1)" == Ok (testField "post" |> withArgs [("id", IntValue 1)])
expect
    parseStr field "firstPost: post(id: 1)"
    == Ok
        (
            testField "post"
            |> withAlias "firstPost"
            |> withArgs [("id", IntValue 1)]
        )
expect
    parseStr field "user { name, age }"
    == Ok
        (
            testField "user"
            |> withSelection [
                testField "name",
                testField "age",
            ]
        )
expect
    parseStr field "viewer: user { id posts(status: ACTIVE, after: \"2023-10-04\") { id title } name }"
    == Ok
        (
            testField "user"
            |> withAlias "viewer"
            |> withSelection [
                testField "id",
                testField "posts"
                |> withArgs [
                    ("status", EnumValue "ACTIVE"),
                    ("after", StringValue "2023-10-04"),
                ]
                |> withSelection [
                    testField "id",
                    testField "title",
                ],
                testField "name",
            ]
        )

# Argument

Argument : (Str, Value)

arguments : Parser RawStr (List Argument)
arguments =
    const identity
    |> skip (codeunit '(')
    |> skip ignored
    |> keep (argument |> sepBy1 ignored)
    |> skip ignored
    |> skip (codeunit ')')

argument : Parser RawStr Argument
argument =
    const \k -> \v -> (k, v)
    |> keep name
    |> skip ignored
    |> skip (codeunit ':')
    |> skip ignored
    |> keep value

# Value

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

value : Parser RawStr Value
value =
    oneOf [
        variable,
        intValue,
        stringValue,
        booleanValue,
        nullValue,
        enumValue,
        listValue,
        objectValue,
    ]

expect parseStr value "$id" == Ok (Variable "id")
expect parseStr value "123" == Ok (IntValue 123)
expect parseStr value "-456" == Ok (IntValue -456)
expect parseStr value "\"hello world\"" == Ok (StringValue "hello world")
expect parseStr value "\"hello\\nworld\"" == Ok (StringValue "hello\nworld")
expect parseStr value "\"my name is \\\"Agus\\\"\"" == Ok (StringValue "my name is \"Agus\"")
expect parseStr value "true" == Ok (BooleanValue Bool.true)
expect parseStr value "false" == Ok (BooleanValue Bool.false)
expect parseStr value "null" == Ok NullValue
expect parseStr value "ACTIVE" == Ok (EnumValue "ACTIVE")
expect parseStr value "suspended" == Ok (EnumValue "suspended")
expect parseStr value "[]" == Ok (ListValue [])
expect parseStr value "[ $id1, $id2 ]" == Ok (ListValue [Variable "id1", Variable "id2"])
expect parseStr value "[42, 123, 234]" == Ok (ListValue [IntValue 42, IntValue 123, IntValue 234])
expect parseStr value "[\"john\", \"Mike\"]" == Ok (ListValue [StringValue "john", StringValue "Mike"])
expect parseStr value "{}" == Ok (ObjectValue [])
expect
    parseStr value "{ id: $id name: \"John\", age :56, status: ACTIVE }"
    == Ok
        (
            ObjectValue [
                ("id", Variable "id"),
                ("name", StringValue "John"),
                ("age", IntValue 56),
                ("status", EnumValue "ACTIVE"),
            ]
        )

# Value: Variable

variable : Parser RawStr Value
variable =
    const Variable
    |> skip (codeunit '$')
    |> keep name

# Value: Int

intValue : Parser RawStr Value
intValue =
    # TODO: Be more strict about leading zeroes
    const \neg -> \num -> if Result.isOk neg then IntValue -num else IntValue num
    |> keep (maybe (codeunit '-'))
    |> keep ParserStr.positiveInt

# Value: String

stringValue : Parser RawStr Value
stringValue =
    # TODO: Block strings
    chars <-
        many stringChar
        |> between (codeunit '"') (codeunit '"')
        |> andThen

    when Str.fromUtf8 chars is
        Ok str ->
            const (StringValue str)

        Err (BadUtf8 _ _) ->
            fail "String value is not valid UTF8"

stringChar : Parser RawStr U8
stringChar =
    oneOf [
        codeunitSatisfies \char ->
            (char != '"')
            && (char != '\\')
            && (char != '\n')
            && (char != '\r'),
        # TODO: Escaped unicode
        const identity
        |> skip (codeunit '\\')
        |> keep escapedChar,
    ]

escapedChar : Parser RawStr U8
escapedChar =
    oneOf [
        codeunit '"',
        codeunit '\\',
        codeunit '/',
        codeunit 'b' |> map \_ -> 0x08,
        codeunit 'f' |> map \_ -> 0x0c,
        codeunit 'n' |> map \_ -> '\n',
        codeunit 'r' |> map \_ -> '\r',
        codeunit 't' |> map \_ -> '\t',
    ]

# Value: Boolean

booleanValue : Parser RawStr Value
booleanValue =
    oneOf [
        string "true" |> map \_ -> BooleanValue Bool.true,
        string "false" |> map \_ -> BooleanValue Bool.false,
    ]

# Value: Null

nullValue : Parser RawStr Value
nullValue =
    string "null" |> map \_ -> NullValue

# Value: Enum

enumValue : Parser RawStr Value
enumValue =
    # No need to check for true/false/null because it would never get here
    name |> map EnumValue

# Value: List

listValue : Parser RawStr Value
listValue =
    const ListValue
    |> skip (codeunit '[')
    |> skip ignored
    |> keep (recursiveValue |> sepBy ignored)
    |> skip ignored
    |> skip (codeunit ']')

# Value: Object

objectValue : Parser RawStr Value
objectValue =
    const ObjectValue
    |> skip (codeunit '{')
    |> skip ignored
    |> keep (objectField |> sepBy ignored)
    |> skip ignored
    |> skip (codeunit '}')

objectField : Parser RawStr (Str, Value)
objectField =
    const \k -> \v -> (k, v)
    |> keep name
    |> skip ignored
    |> skip (codeunit ':')
    |> skip ignored
    |> keep recursiveValue

# Name

name : Parser RawStr Str
name =
    start, continue <- map2 nameStart nameContinue

    continue
    |> List.prepend start
    |> strFromRaw

expect parseStr name "name" == Ok "name"
expect parseStr name "productId" == Ok "productId"
expect parseStr name "User" == Ok "User"
expect parseStr name "__typename" == Ok "__typename"
expect parseStr name "users2" == Ok "users2"
expect parseStr name "2users" |> Result.isErr
expect parseStr name "product_id" == Ok "product_id"
expect parseStr name "product id" |> Result.isErr

nameStart =
    isAlpha
    |> orUnderscore
    |> codeunitSatisfies

nameContinue =
    isAlphanumeric
    |> orUnderscore
    |> codeunitSatisfies
    |> oneOrMore

orUnderscore = \option -> \code -> option code || code == '_'

isAlpha = \code ->
    (code >= 'A' && code <= 'Z') || (code >= 'a' && code <= 'z')

isNumeric = \code ->
    code >= '0' && code <= '9'

isAlphanumeric = \code ->
    isAlpha code || isNumeric code

# Test field helpers

testField = \fname -> Field { field: fname, alias: Err Nothing, arguments: [], selectionSet: [] }
withSelection = \Field fiel, ss -> Field { fiel & selectionSet: ss }
withAlias = \Field fiel, alias -> Field { fiel & alias: Ok alias }
withArgs = \Field fiel, args -> Field { fiel & arguments: args }

# Helpers

ignored =
    # TODO: Comments
    many ignoredCodeunit

ignoredCodeunit =
    # TODO: Unicode BOM
    codeunitSatisfies \c ->
        (c == ' ')
        || (c == '\t')
        || (c == '\n')
        || (c == '\r')
        || (c == ',')

identity = \x -> x

withDefault : Parser input (Result a [Nothing]), a -> Parser input a
withDefault = \parser, def ->
    parser |> map (\m -> Result.withDefault m def)

# Workaround for: https://github.com/roc-lang/roc/issues/5682

recursiveSelectionSet : Parser RawStr (List Selection)
recursiveSelectionSet =
    ParserCore.buildPrimitiveParser (\input -> ParserCore.parsePartial selectionSet input)

recursiveValue : Parser RawStr Value
recursiveValue =
    ParserCore.buildPrimitiveParser (\input -> ParserCore.parsePartial value input)

recursiveType : Parser RawStr Type
recursiveType =
    ParserCore.buildPrimitiveParser (\input -> ParserCore.parsePartial type input)

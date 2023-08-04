interface Gql.Parse
    exposes [parseDocument]
    imports [
        Parser.Core.{
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
        Parser.Str.{
            RawStr,
            parseStr,
            strFromRaw,
            codeunit,
            codeunitSatisfies,
            oneOf,
            string,
        },
        Gql.Document.{
            Document,
            Definition,
            OperationType,
            VariableDefinition,
            Type,
            NamedOrListType,
            Selection,
            Argument,
            Value,
        },
    ]

# https://spec.graphql.org/October2021

parseDocument : Str -> Result Document [ParsingFailure Str, ParsingIncomplete Str]
parseDocument = \input ->
    parseStr document input

# Document

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
                posts {
                    ...PostDetails
                }
            }
            ... {
                sessionId
            }
        }

        query Posts($active: Boolean!, $after: Date) {
            posts(active: $active, before: "2021", after: $after) {
                ...PostDetails
                status
            }
        }

        fragment PostDetails on Post {
            id 
            title
            body {
                __typename
                ... on Text {
                    text
                }
                ... on Image {
                    imageUrl
                }
            }
        }
        """
    == Ok [
        Operation {
            type: Query,
            name: Ok "GetUser",
            variables: [],
            selectionSet: [
                testField "me"
                |> withSelection [
                    testField "id",
                    testField "name",
                    testField "posts" |> withSelection [FragmentSpread "PostDetails"],
                ],
                InlineFragment {
                    typeName: Err Nothing,
                    selectionSet: [testField "sessionId"],
                },
            ],
        },
        Operation {
            type: Query,
            name: Ok "Posts",
            variables: [
                { name: "active", type: NonNull (Named "Boolean"), default: Err Nothing },
                { name: "after", type: Nullable (Named "Date"), default: Err Nothing },
            ],
            selectionSet: [
                testField "posts"
                |> withArgs [
                    ("active", Var "active"),
                    ("before", String "2021"),
                    ("after", Var "after"),
                ]
                |> withSelection [
                    FragmentSpread "PostDetails",
                    testField "status",
                ],
            ],
        },
        Fragment {
            name: "PostDetails",
            typeName: "Post",
            selectionSet: [
                testField "id",
                testField "title",
                testField "body"
                |> withSelection [
                    testField "__typename",
                    InlineFragment {
                        typeName: Ok "Text",
                        selectionSet: [testField "text"],
                    },
                    InlineFragment {
                        typeName: Ok "Image",
                        selectionSet: [testField "imageUrl"],
                    },
                ],
            ],
        },
    ]

# Definition

definition : Parser RawStr Definition
definition =
    oneOf [
        operationDefinition,
        fragmentDefinition,
    ]

# Operation Definition

operationDefinition : Parser RawStr Definition
operationDefinition =
    const \typ -> \nam -> \vars -> \ss -> Operation { type: typ, name: nam, variables: vars, selectionSet: ss }
    |> keep (maybe opType |> withDefault Query)
    |> skip ignored
    |> keep (maybe name)
    |> skip ignored
    |> keep (maybe variableDefinitions |> withDefault [])
    |> skip ignored
    |> keep selectionSet

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
                variables: [],
                selectionSet: [testField "user" |> withSelection [testField "id"]],
            }
        )
expect
    parseStr operationDefinition "query GetUser($id: ID!) { user(id: $id) { id } }"
    == Ok
        (
            Operation {
                type: Query,
                name: Ok "GetUser",
                variables: [
                    {
                        name: "id",
                        type: NonNull (Named "ID"),
                        default: Err Nothing,
                    },
                ],
                selectionSet: [
                    testField "user"
                    |> withArgs [
                        ("id", Var "id"),
                    ]
                    |> withSelection [testField "id"],
                ],
            }
        )
expect
    parseStr operationDefinition "mutation LogOut { logOut { success } }"
    == Ok
        (
            Operation {
                type: Mutation,
                name: Ok "LogOut",
                variables: [],
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
                variables: [],
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
                variables: [],
                selectionSet: [testField "user" |> withSelection [testField "id"]],
            }
        )

# Variable Definition

variableDefinitions : Parser RawStr (List VariableDefinition)
variableDefinitions =
    const identity
    |> skip (codeunit '(')
    |> skip ignored
    |> keep (variableDefinition |> sepBy1 ignored)
    |> skip ignored
    |> skip (codeunit ')')

variableDefinition : Parser RawStr VariableDefinition
variableDefinition =
    const \vname -> \typ -> \dv -> { name: vname, type: typ, default: dv }
    |> keep variable
    |> skip ignored
    |> skip (codeunit ':')
    |> skip ignored
    |> keep type
    |> skip ignored
    |> keep (maybe defaultValue)

expect
    parseStr variableDefinition "$id: ID"
    == Ok {
        name: "id",
        type: Nullable (Named "ID"),
        default: Err Nothing,
    }
expect
    parseStr variableDefinition "$active: Boolean! = true"
    == Ok {
        name: "active",
        type: NonNull (Named "Boolean"),
        default: Ok (Boolean Bool.true),
    }
expect
    parseStr variableDefinition "$ids :[ID!]= [\"1\", \"2\"]"
    == Ok {
        name: "ids",
        type: Nullable (ListT (NonNull (Named "ID"))),
        default: Ok (List [String "1", String "2"]),
    }

variable : Parser RawStr Str
variable =
    const identity
    |> skip (codeunit '$')
    |> keep name

defaultValue : Parser RawStr Value
defaultValue =
    const identity
    |> skip (codeunit '=')
    |> skip ignored
    |> keep value

# Type

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
    |> keep typeCondition
    |> skip ignored
    |> keep selectionSet

typeCondition : Parser RawStr Str
typeCondition =
    const identity
    |> skip (string "on")
    |> skip ignored
    |> keep name

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

selection : Parser RawStr Selection
selection =
    oneOf [
        field,
        fragmentSpread,
        recursiveInlineFragment,
    ]

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
expect parseStr selectionSet "{ ... PostDetails }" == Ok [FragmentSpread "PostDetails"]
expect parseStr selectionSet "{ ... on Post { id } }" == Ok [InlineFragment { typeName: Ok "Post", selectionSet: [testField "id"] }]
expect
    parseStr
        selectionSet
        """
        {
            ...UserDetails,
            fullName: name, 
              email
            phone
            ... on Admin {
                permissions
            }
        }
        """
    == Ok [
        FragmentSpread "UserDetails",
        testField "name" |> withAlias "fullName",
        testField "email",
        testField "phone",
        InlineFragment {
            typeName: Ok "Admin",
            selectionSet: [
                testField "permissions",
            ],
        },
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
expect parseStr field "post(id: 1)" == Ok (testField "post" |> withArgs [("id", Int 1)])
expect
    parseStr field "firstPost: post(id: 1)"
    == Ok
        (
            testField "post"
            |> withAlias "firstPost"
            |> withArgs [("id", Int 1)]
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
                    ("status", Enum "ACTIVE"),
                    ("after", String "2023-10-04"),
                ]
                |> withSelection [
                    testField "id",
                    testField "title",
                ],
                testField "name",
            ]
        )

# Argument

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

value : Parser RawStr Value
value =
    oneOf [
        variableValue,
        intValue,
        stringValue,
        booleanValue,
        nullValue,
        enumValue,
        listValue,
        objectValue,
        # TODO:
        # floatValue,
    ]

expect parseStr value "$id" == Ok (Var "id")
expect parseStr value "123" == Ok (Int 123)
expect parseStr value "-456" == Ok (Int -456)
expect parseStr value "\"hello world\"" == Ok (String "hello world")
expect parseStr value "\"hello\\nworld\"" == Ok (String "hello\nworld")
expect parseStr value "\"my name is \\\"Agus\\\"\"" == Ok (String "my name is \"Agus\"")
expect parseStr value "true" == Ok (Boolean Bool.true)
expect parseStr value "false" == Ok (Boolean Bool.false)
expect parseStr value "null" == Ok Null
expect parseStr value "ACTIVE" == Ok (Enum "ACTIVE")
expect parseStr value "suspended" == Ok (Enum "suspended")
expect parseStr value "[]" == Ok (List [])
expect parseStr value "[ $id1, $id2 ]" == Ok (List [Var "id1", Var "id2"])
expect parseStr value "[42, 123, 234]" == Ok (List [Int 42, Int 123, Int 234])
expect parseStr value "[\"john\", \"Mike\"]" == Ok (List [String "john", String "Mike"])
expect parseStr value "{}" == Ok (Object [])
expect
    parseStr value "{ id: $id name: \"John\", age :56, status: ACTIVE }"
    == Ok
        (
            Object [
                ("id", Var "id"),
                ("name", String "John"),
                ("age", Int 56),
                ("status", Enum "ACTIVE"),
            ]
        )

# Value: Var

variableValue : Parser RawStr Value
variableValue =
    const Var
    |> skip (codeunit '$')
    |> skip ignored
    |> keep name

# Value: Int

intValue : Parser RawStr Value
intValue =
    # TODO: Be more strict about leading zeroes
    const \neg -> \num -> if Result.isOk neg then Int -num else Int num
    |> keep (maybe (codeunit '-'))
    |> keep Parser.Str.positiveInt

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
            const (String str)

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
        string "true" |> map \_ -> Boolean Bool.true,
        string "false" |> map \_ -> Boolean Bool.false,
    ]

# Value: Null

nullValue : Parser RawStr Value
nullValue =
    string "null" |> map \_ -> Null

# Value: Enum

enumValue : Parser RawStr Value
enumValue =
    # No need to check for true/false/null because it would never get here
    name |> map Enum

# Value: List

listValue : Parser RawStr Value
listValue =
    const List
    |> skip (codeunit '[')
    |> skip ignored
    |> keep (recursiveValue |> sepBy ignored)
    |> skip ignored
    |> skip (codeunit ']')

# Value: Object

objectValue : Parser RawStr Value
objectValue =
    const Object
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

# Fragment Spread

fragmentSpread : Parser RawStr Selection
fragmentSpread =
    const FragmentSpread
    |> skip (string "...")
    |> skip ignored
    |> keep fragmentName

# Inline Fragment

inlineFragment : Parser RawStr Selection
inlineFragment =
    const \typ -> \ss -> InlineFragment { typeName: typ, selectionSet: ss }
    |> skip (string "...")
    |> skip ignored
    |> keep (maybe typeCondition)
    |> skip ignored
    |> keep recursiveSelectionSet

expect parseStr inlineFragment "... on User { id }" == Ok (InlineFragment { typeName: Ok "User", selectionSet: [testField "id"] })
expect parseStr inlineFragment "... { id }" == Ok (InlineFragment { typeName: Err Nothing, selectionSet: [testField "id"] })
expect
    parseStr inlineFragment "... on Post { id ...PostDetails }"
    == Ok
        (
            InlineFragment {
                typeName: Ok "Post",
                selectionSet: [
                    testField "id",
                    FragmentSpread "PostDetails",
                ],
            }
        )

# Name

name : Parser RawStr Str
name =
    start, maybeContinue <- map2 nameStart (maybe nameContinue)

    when maybeContinue is
        Ok continue ->
            continue
            |> List.prepend start
            |> strFromRaw

        Err Nothing ->
            strFromRaw [start]

expect parseStr name "x" == Ok "x"
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
    Parser.Core.buildPrimitiveParser (\input -> Parser.Core.parsePartial selectionSet input)

recursiveInlineFragment : Parser RawStr Selection
recursiveInlineFragment =
    Parser.Core.buildPrimitiveParser (\input -> Parser.Core.parsePartial inlineFragment input)

recursiveValue : Parser RawStr Value
recursiveValue =
    Parser.Core.buildPrimitiveParser (\input -> Parser.Core.parsePartial value input)

recursiveType : Parser RawStr Type
recursiveType =
    Parser.Core.buildPrimitiveParser (\input -> Parser.Core.parsePartial type input)

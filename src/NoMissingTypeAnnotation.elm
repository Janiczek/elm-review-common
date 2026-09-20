module NoMissingTypeAnnotation exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location)
import Elm.TypeInference.Type exposing (Type)
import Review.Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports top-level declarations that do not have a type annotation.

Type annotations help you understand what happens in the code, and it will help the compiler give better error messages.

    config =
        [ NoMissingTypeAnnotation.rule
        ]

This rule does not report declarations without a type annotation inside a `let in`.
For that, enable [`NoMissingTypeAnnotationInLetIn`](./NoMissingTypeAnnotationInLetIn).


## Fail

    a =
        1


## Success

    a : number
    a =
        1

    b : number
    b =
        let
            c =
                2
        in
        c


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-common/example --rules NoMissingTypeAnnotation
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingTypeAnnotation" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        name : Node String
                        name =
                            function.declaration
                                |> Node.value
                                |> .name

                        nameString : String
                        nameString =
                            Node.value name

                        start : Location
                        start =
                            function.declaration
                                |> Node.range
                                |> .start

                        inferredType : Type
                        inferredType =
                            Elm.TypeInference.Type.Unit
                    in
                    [ Rule.errorWithFix
                        { message = "Missing type annotation for `" ++ nameString ++ "`"
                        , details =
                            [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                            ]
                        }
                        (Node.range name)
                        [ Review.Fix.insertAt start
                            ("{NAME} : {TYPE}\n"
                                |> String.replace "{NAME}" nameString
                                |> String.replace "{TYPE}" (Elm.TypeInference.Type.toString inferredType)
                            )
                        ]
                    ]

                Just _ ->
                    []

        _ ->
            []

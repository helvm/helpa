[ Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermMinus
                ( TermSymbol
                    ( Literal 1 )
                )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermExpression
                ( Expression Nothing
                    ( TermMinus
                        ( TermSymbol
                            ( Literal 1 )
                        )
                    )
                )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermExpression
                ( Expression Nothing
                    ( TermMinus
                        ( TermSymbol
                            ( Literal 1 )
                        )
                    )
                )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 0 )
            )
        )
    ]
, Instruction Data
    [ ItemLabel "a"
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 0 )
            )
        )
    ]
]
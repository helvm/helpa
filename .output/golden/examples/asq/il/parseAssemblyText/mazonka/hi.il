[ Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Hi" )
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
        ( Expression
            ( Just
                ( PMExpression Plus
                    ( Expression Nothing
                        ( TermSymbol
                            ( Literal 1 )
                        )
                    )
                )
            )
            ( TermSymbol
                ( Variable "Hi" )
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
                ( Literal 0 )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 0 )
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
, Instruction Code []
, Instruction Data
    [ ItemLabel "Hi"
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 72 )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 105 )
            )
        )
    ]
]
[ Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "p" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    ]
, Instruction Code
    [ ItemLabel "a"
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
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "m1" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "p" )
            )
        )
    ]
, Instruction Code []
, Instruction Code []
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "E" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "a" )
            )
        )
    ]
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    ]
, Instruction Code []
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "p" )
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
, Instruction Code []
, Instruction Code
    [ ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "Z" )
            )
        )
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 0 )
            )
        )
    ]
, Instruction Code []
, Instruction Data
    [ ItemLabel "p"
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "H" )
            )
        )
    , ItemLabel "Z"
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Literal 0 )
            )
        )
    , ItemLabel "m1"
    , ItemExpression
        ( Expression Nothing
            ( TermMinus
                ( TermSymbol
                    ( Literal 1 )
                )
            )
        )
    ]
, Instruction Code []
, Instruction Data
    [ ItemLabel "H"
    , ItemString "Hello, World!
      "
    , ItemLabel "E"
    , ItemExpression
        ( Expression Nothing
            ( TermSymbol
                ( Variable "E" )
            )
        )
    ]
]
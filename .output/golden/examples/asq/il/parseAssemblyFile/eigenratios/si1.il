[ Instruction
    ( Just "START" ) Nothing
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "A1" )
            ( Variable "A1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "PC" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "A1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "A" )
            ( Variable "A" ) Nothing
        )
    )
, Instruction
    ( Just "A1" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "ZERO" )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "A2" )
        )
    )
, Instruction
    ( Just "A2" )
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "A" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "LEN" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "A" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "B1" )
            ( Variable "B1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "PC" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "B1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "NEG1" )
            ( Variable "B1" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "B" )
            ( Variable "B" ) Nothing
        )
    )
, Instruction
    ( Just "B1" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "ZERO" )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "B2" )
        )
    )
, Instruction
    ( Just "B2" )
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "B" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "LEN" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "B" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "C1" )
            ( Variable "C1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "PC" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "C1" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "NEG2" )
            ( Variable "C1" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "C" )
            ( Variable "C" ) Nothing
        )
    )
, Instruction
    ( Just "C1" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "ZERO" )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "C2" )
        )
    )
, Instruction
    ( Just "C2" )
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "C" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction
    ( Just "A" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction
    ( Just "B" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction Nothing
    ( Just
        ( Data
            ( Variable "LEQZ" )
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "NEG3" )
            ( Variable "PC" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" )
            ( Just
                ( Variable "START" )
            )
        )
    )
, Instruction Nothing Nothing
, Instruction
    ( Just "LEQZ" ) Nothing
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "PC" )
            ( Variable "PC" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "C" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "PC" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "NEG1" )
            ( Variable "C" )
            ( Just
                ( Literal
                    ( - 1 )
                )
            )
        )
    )
, Instruction Nothing Nothing
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "LEN" )
            ( Variable "ZERO" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "PC" ) Nothing
        )
    )
, Instruction Nothing
    ( Just
        ( Code
            ( Variable "ZERO" )
            ( Variable "ZERO" )
            ( Just
                ( Variable "START" )
            )
        )
    )
, Instruction Nothing Nothing
, Instruction
    ( Just "NEG3" )
    ( Just
        ( Data
            ( Literal
                ( - 3 )
            )
        )
    )
, Instruction
    ( Just "NEG2" )
    ( Just
        ( Data
            ( Literal
                ( - 2 )
            )
        )
    )
, Instruction
    ( Just "NEG1" )
    ( Just
        ( Data
            ( Literal
                ( - 1 )
            )
        )
    )
, Instruction
    ( Just "ZERO" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction Nothing Nothing
, Instruction
    ( Just "C" )
    ( Just
        ( Data
            ( Literal 0 )
        )
    )
, Instruction
    ( Just "PC" )
    ( Just
        ( Data
            ( Variable "PROG" )
        )
    )
, Instruction
    ( Just "LEN" )
    ( Just
        ( Data
            ( Variable "PROG" )
        )
    )
, Instruction Nothing Nothing
, Instruction
    ( Just "PROG" ) Nothing
]
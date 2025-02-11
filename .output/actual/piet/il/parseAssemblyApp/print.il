[ Def [] "print"
    [ Micro ( Command Dup )
    , Micro
        ( Directive
            ( Label
                ( LNatural 0 )
            )
        )
    , Micro
        ( Directive
            ( Branch ( Just Pop ) ( Just BZ )
                ( BLNatural ( Just Forward ) 1 )
            )
        )
    , Micro
        ( Directive
            ( PushInteger 2 )
        )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Roll )
    , Micro ( Command Out )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Sub )
    , Micro
        ( Directive
            ( Branch ( Just Dup ) Nothing
                ( BLNatural ( Just Backward ) 0 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( LNatural 1 )
            )
        )
    ]
]
[ Def [] "readln"
    [ Micro
        ( Directive
            ( PushInteger 0 )
        )
    , Micro
        ( Directive
            ( Branch Nothing Nothing
                ( BLNatural ( Just Forward ) 0 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( LNatural 1 )
            )
        )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( Branch Nothing ( Just BZ )
                ( BLNatural ( Just Forward ) 0 )
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
    , Micro ( Command Pop )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Sub )
    , Micro
        ( Directive
            ( Label
                ( LNatural 0 )
            )
        )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Add )
    , Micro ( Command Dup )
    , Micro ( Command In )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( PushInteger 8 )
        )
    , Micro ( Command Sub )
    , Micro
        ( Directive
            ( Branch ( Just Pop ) ( Just BZ )
                ( BLNatural ( Just Backward ) 1 )
            )
        )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( PushInteger 10 )
        )
    , Micro ( Command Sub )
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
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Add )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Roll )
    , Micro
        ( Directive
            ( Branch Nothing Nothing
                ( BLNatural ( Just Backward ) 0 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( LNatural 1 )
            )
        )
    , Micro ( Command Pop )
    , Micro
        ( Directive
            ( PushInteger 1 )
        )
    , Micro ( Command Sub )
    ]
]
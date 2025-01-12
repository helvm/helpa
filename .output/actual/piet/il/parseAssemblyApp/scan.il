[ Def [] "scan"
    [ Micro
        ( Directive
            ( Label
                ( Literal 1 )
            )
        )
    , Micro ( Command In )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( PushInteger 7 )
        )
    , Micro
        ( Directive
            ( Branch Nothing ( Just BLE )
                ( BLNatural ( Just Forward ) 1 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( Literal 2 )
            )
        )
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
    , Micro
        ( Directive
            ( PushInteger 10 )
        )
    , Micro ( Command Sub )
    , Micro
        ( Directive
            ( Branch Nothing ( Just BNZ )
                ( BLNatural ( Just Backward ) 2 )
            )
        )
    , Micro
        ( Directive
            ( PushInteger 48 )
        )
    , Micro ( Command Sub )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( PushInteger 0 )
        )
    , Micro
        ( Directive
            ( Branch Nothing ( Just BGT )
                ( BLNatural ( Just Forward ) 2 )
            )
        )
    , Micro ( Command Pop )
    , Micro
        ( Directive
            ( PushInteger 0 )
        )
    , Micro
        ( Directive
            ( Branch Nothing Nothing
                ( BLNatural ( Just Forward ) 1 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( Literal 2 )
            )
        )
    , Micro ( Command Dup )
    , Micro
        ( Directive
            ( PushInteger 7 )
        )
    , Micro
        ( Directive
            ( Branch Nothing ( Just BLE )
                ( BLNatural ( Just Forward ) 1 )
            )
        )
    , Micro ( Command Pop )
    , Micro
        ( Directive
            ( PushInteger 0 )
        )
    , Micro
        ( Directive
            ( Branch Nothing Nothing
                ( BLNatural ( Just Forward ) 1 )
            )
        )
    , Micro
        ( Directive
            ( Label
                ( Literal 1 )
            )
        )
    ]
]
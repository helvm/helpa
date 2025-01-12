[ Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "a dusty, dim hallway.
        There is a door to the kitchen here.
        There is a door to the bedroom here.
        There is a rickety loft ladder here.
        " )
    )
, Micro
    ( Directive
        ( Label
            ( Variable "entry" )
        )
    )
, Micro
    ( Directive
        ( Print "
        Where do you want to go today?
        1 - kitchen
        2 - bedroom
        3 - loft
        " )
    )
, Call [] "scan"
, Micro
    ( Directive
        ( PushInteger 10 )
    )
, Micro ( Command Out )
, Micro
    ( Directive
        ( BranchTable
            [ BLNatural ( Just Forward ) 0
            , BLNatural ( Just Forward ) 1
            , BLNatural ( Just Forward ) 2
            , BLNatural ( Just Forward ) 3
            ]
        )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 0 )
        )
    )
, Micro
    ( Directive
        ( PushString "
        Please select 1, 2 or 3.
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "entry" )
        )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 1 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 2 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 3 )
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
            ( BLIdentifier "_track_0" )
        )
    )
]
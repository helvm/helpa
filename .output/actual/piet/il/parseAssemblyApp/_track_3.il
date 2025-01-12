[ Micro ( Directive Track )
, Micro
    ( Directive
        ( PushString "a well-stocked kitchen.
        It smells invitingly of apple pancake.
        Your wife is here.
        She gives you a look.
        " )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 3 )
        )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Print "
        Your options:
        1 - talk to her
        2 - go back to the hallway
        " )
    )
, Call [] "scan"
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Sub )
, Micro
    ( Directive
        ( BranchTable
            [ BLNatural ( Just Forward ) 0
            , BLNatural ( Just Forward ) 4
            ]
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushString "Huh...?
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Backward ) 3 )
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
        ( PushInteger 2 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 4 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( Branch Nothing ( Just BGT )
            ( BLNatural ( Just Forward ) 0 )
        )
    )
, Micro ( Command Pop )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 6 )
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
            ( Literal 0 )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 8 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( PushInteger 3 )
    )
, Micro
    ( Directive
        ( Branch Nothing ( Just BGT )
            ( BLNatural ( Just Forward ) 0 )
        )
    )
, Micro ( Command Pop )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 7 )
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
            ( Literal 0 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BZ )
            ( BLNatural ( Just Forward ) 0 )
        )
    )
, Micro
    ( Directive
        ( PushString "Wife: I hope you're not going to paint on the bedsheets again.
        You look guilty.
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Backward ) 3 )
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
        ( PushString "Wife: Go on, now, off with you.
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Backward ) 3 )
        )
    )
, Micro
    ( Directive
        ( Label
            ( Literal 4 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
]
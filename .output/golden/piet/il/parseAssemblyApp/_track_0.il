[ Micro
    ( Directive
        ( Label
            ( LIdentifier "_track_0" )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 9 )
    )
, Micro ( Command Sub )
, Micro
    ( Directive
        ( Branch Nothing ( Just BZ )
            ( BLIdentifier "_track_10" )
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
        ( Branch Nothing ( Just BZ )
            ( BLIdentifier "_track_11" )
        )
    )
, Micro
    ( Directive
        ( Print "
        You find yourself in " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( BranchTable
            [ BLIdentifier "_track_1"
            , BLIdentifier "_track_2"
            , BLIdentifier "_track_3"
            , BLIdentifier "_track_4"
            , BLIdentifier "_track_5"
            , BLIdentifier "_track_6"
            , BLIdentifier "_track_7"
            , BLIdentifier "_track_8"
            , BLIdentifier "_track_9"
            ]
        )
    )
, Micro
    ( Directive
        ( Print "error." )
    )
, Micro ( Command OutN )
, Micro
    ( Directive
        ( PushInteger 32 )
    )
, Micro ( Command Out )
, Micro ( Command OutN )
, Micro
    ( Directive
        ( PushInteger 10 )
    )
, Micro ( Command Out )
, Micro ( Directive Halt )
]
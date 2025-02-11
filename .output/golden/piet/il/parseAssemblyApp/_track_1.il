[ Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "a rather dark studio.
        There is an easel here.
        There is a ladder leading down.

        " )
    )
, Micro
    ( Directive
        ( Label
            ( LIdentifier "entry" )
        )
    )
, Micro
    ( Directive
        ( PushString "Please select:

        1 - paint
        2 - go down ladder
        " )
    )
, Call [] "print"
, Call [] "scan"
, Micro
    ( Directive
        ( BranchTable
            [ BLNatural ( Just Forward ) 0 ]
        )
    )
]
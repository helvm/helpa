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
, Def [] "print"
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
, Micro
    ( Directive
        ( PushInteger 16 )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 0 )
        )
    )
, Micro
    ( Directive
        ( Print "=" )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Sub )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 0 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( Print "
        = Piet's Quest =
        " )
    )
, Micro
    ( Directive
        ( PushInteger 16 )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 0 )
        )
    )
, Micro
    ( Directive
        ( Print "=" )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Sub )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 0 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( Print "

        " )
    )
, Micro
    ( Directive
        ( PushInteger 0 )
    )
, Micro
    ( Directive
        ( PushInteger 0 )
    )
, Micro
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
, Def [] "scan"
    [ Micro
        ( Directive
            ( Label
                ( LNatural 1 )
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
                ( LNatural 2 )
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
                ( LNatural 2 )
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
                ( LNatural 1 )
            )
        )
    ]
, Micro ( Directive Track )
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
, Micro
    ( Directive
        ( Label
            ( LNatural 0 )
        )
    )
, Micro
    ( Directive
        ( PushString "
        Please select 1 or 2.
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
            ( LNatural 2 )
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
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
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
        ( PushInteger 7 )
    )
, Micro ( Command Sub )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 2 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 4 )
    )
, Micro
    ( Directive
        ( Branch ( Just Add ) Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 2 )
        )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 2 )
        )
    )
, Micro
    ( Directive
        ( PushString "You have no canvas.
        " )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Label
            ( LNatural 2 )
        )
    )
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
            ( BLNatural ( Just Forward ) 2 )
        )
    )
, Micro
    ( Directive
        ( PushString "You have no brush.
        " )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Label
            ( LNatural 2 )
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
            ( BLNatural ( Just Forward ) 2 )
        )
    )
, Micro
    ( Directive
        ( PushString "You have no idea what to paint.
        " )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Label
            ( LNatural 2 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "entry" )
        )
    )
, Micro ( Directive Track )
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
            ( LIdentifier "entry" )
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
            [ BLNatural ( Just Forward ) 0 ]
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 0 )
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
            ( LNatural 1 )
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
            ( LNatural 2 )
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
            ( LNatural 3 )
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
, Micro ( Directive Track )
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
            ( LNatural 3 )
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
            [ BLNatural ( Just Forward ) 0 ]
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
            ( LNatural 0 )
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
            ( LNatural 0 )
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
            ( LNatural 0 )
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
            ( LNatural 0 )
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
            ( LNatural 4 )
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
, Micro ( Directive Track )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 1 )
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
, Micro
    ( Directive
        ( PushInteger 0 )
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
        ( PushString "
        " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
, Micro
    ( Directive
        ( PushInteger
            ( -1 )
        )
    )
, Micro ( Command Roll )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
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
        ( PushString "2 - rip off the bedclothes
        " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
, Micro
    ( Directive
        ( PushInteger
            ( -1 )
        )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
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
        ( PushString "
        Your options:
        1 - return to the hallway
        " )
    )
, Micro ( Command Dup )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
, Micro
    ( Directive
        ( PushInteger
            ( -1 )
        )
    )
, Micro ( Command Roll )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Forward ) 1 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushString "Sunbeams warm the carefully made bed.
        " )
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
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro
    ( Directive
        ( PushString "The harsh sunlight exposes the unmade bed.
        You feel a little guilty.
        " )
    )
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
, Micro
    ( Directive
        ( PushString "a cozy bedroom. " )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 4 )
        )
    )
, Call [] "print"
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 4 )
        )
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
            [ BLIdentifier "_track_0"
            , BLNatural ( Just Forward ) 1
            ]
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 3 )
        )
    )
, Micro
    ( Directive
        ( PushInteger 0 )
    )
, Micro
    ( Directive
        ( PushString "I beg your pardon?
        " )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLNatural ( Just Backward ) 4 )
        )
    )
, Micro
    ( Directive
        ( Label
            ( LNatural 1 )
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
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Mod )
, Micro
    ( Directive
        ( Branch Nothing ( Just BNZ )
            ( BLNatural ( Just Backward ) 3 )
        )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 5 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "a trance...

        By and by, you come to.
        Everything is covered in paint.
        You look up.

        You behold your magnificent creation!

        It lacks only a name.

        1 -call your wife

        " )
    )
, Call [] "scan"
, Micro ( Command Pop )
, Micro
    ( Directive
        ( Print "Your long-suffering wife climbs the ladder.
        She winces as she sees the loft.
        Gleefully, you point at your masterpiece.
        She consideres it in silence.

        " )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 9 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "growing excitement.
        You look around sneakily; your eyes begin to glow. You pounce.
        Pounce!
        Feathers fly everywhere.
        You snatch the brilliant white bedsheet and hold it close to your chest.
        You have a white sheet.


        " )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 1 )
    )
, Micro ( Command Add )
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
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "conversation with your wife.
        You: Dear, may I borrow that pastry brush?
        Wife: Not again! You know how hard it is to get the paint out.
        You: Pleeease?
        Wife: ...oh, very well. I hope you realise this means no pie for you tonight.

        You are now the proud owner of a pastry brush.
        " )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 2 )
    )
, Micro ( Command Add )
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
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "conversation with your wife.
        You: Dear, what should I paint next?
        Wife: Oh, you are a silly.
        Wife: Just draw some lines or something.
        Wife: So long as you sign it, they'll pay you.
        You: Lines! There's an idea.

        " )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 8 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( PushString "a quandary.
        Type 1 to continue.
        " )
    )
, Call [] "print"
, Call [] "scan"
, Micro ( Command Pop )
, Micro
    ( Directive
        ( Print "
        Wife: You still look puzzled.
        You: ...what should I call it?
        Wife: Why don't you paint it first, and show me? I'm sure I'll come up with something to solve your puzzlement.
        You: ...you are so sensible.

        You are filled with new purpose!
        " )
    )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 4 )
    )
, Micro ( Command Add )
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
, Micro ( Directive Track )
, Micro
    ( Directive
        ( Print "You: The story of today, of how this work came to be, of all you said to me - it is in these lines.
        You: To one who knows how to read it, everything will be plain.
        Wife: You mean it's some sort of code?
        You: It's art!

        Your wife purses her lips and looks around again.

        1 - continue
        " )
    )
, Call [] "scan"
, Micro ( Command Pop )
, Micro ( Command Pop )
, Micro
    ( Directive
        ( PushInteger 10 )
    )
, Micro
    ( Directive
        ( Branch Nothing Nothing
            ( BLIdentifier "_track_0" )
        )
    )
, Micro ( Directive Track )
, Micro
    ( Directive
        ( PushString "Wife: A mess, that's what I call it. Where's my pastry brush?
        You: Right here!

        You dip the brush and scrawl across the top: MESS


        Your quest is complete.
        " )
    )
, Call [] "print"
, Micro ( Directive Halt )
]
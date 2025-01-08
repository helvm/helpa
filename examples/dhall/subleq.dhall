-- dhall < examples/dhall/subleq.dhall

let Arch: Type = < SQ | WS >

let Register: Type =  < A | B | C | D | Mark >

let registerHandlers =
  { A = "A"
  , B = "B"
  , C = "C"
  , D = "D"
  , Mark = "?"
  }

let A = Register.A
let B = Register.B
let C = Register.C
let D = Register.D
let Mark = Register.Mark

let Operand: Type = < R: Register | I: Integer >
let R = Operand.R
let I = Operand.I

let operandHandlers =
  { R = \(r: Register) -> (merge registerHandlers r)
  , I = Integer/show
  }
let operandShow: Operand -> Text = \(o: Operand) -> merge operandHandlers o


let Count: Type = Natural

let Env: Type =
  { count: Count
  }
let makeEnv = {count = 0}

let State: Type =
      { env : Env
      , result : Text
      }
let makeState = \(env: Env) -> {env = env, result = ""}

let Instruction: Type = Env -> State
let InstructionList: Type = List Instruction

let reduce : Instruction -> State -> State =
      \(i : Instruction) ->
      \(state : State) ->
        let iState = i state.env
        let result = state.result ++ iState.result
        let env = iState.env
        in  { env = env, result = result }

let run : InstructionList -> Instruction =
      \(l : InstructionList) ->
      \(env: Env) ->
        List/fold Instruction l State reduce (makeState env)

let subleq : Operand -> Operand -> Operand -> Instruction =
      \(s : Operand) ->
      \(d : Operand) ->
      \(l : Operand) ->
      \(env : Env) ->
        let sText: Text = (operandShow s)
        let dText: Text = (operandShow d)
        let lText: Text = (operandShow l)
        let result : Text =
              ''
              subleq ${sText} ${dText} ${lText}
              ''
        in  { env = env, result = result }

let sub : Operand -> Operand -> Instruction =
      \(s : Operand) ->
      \(d : Operand) ->
        subleq s d (Operand.R Register.Mark)

let clr : Operand -> Instruction =
      \(s : Operand) -> sub s s

let jmp : Operand -> Instruction =
      \(l : Operand) -> subleq (Operand.I +0) (Operand.I +0) l

let add : Operand -> Operand -> Instruction =
      \(s : Operand) ->
      \(d : Operand) ->
      run
        [ sub s (I +0)
        , sub (I +0) d
        , clr (I +0)
        ]

let mov : Operand -> Operand -> Instruction =
      \(s : Operand) ->
      \(d : Operand) ->
      run
        [ clr d
        , add s d
        ]

in run
    [ mov (R A) (R B)
    , add (R A) (R B)
    ]
    makeEnv

type lable = string

type imm = Lit of int32
         | Lable of lable

type opcode  = NOP | JCN | FIM | SRC | FIN | JIN | JUN | JMS | INC | ISZ
             | ADD | SUB | LD  | XCH | BBL | LDM
             | WRM | WMP | WRR | WPM | WR0 | WR1 | WR2 | WR3
             | SBM | RDM | RDR | ADM | RD0 | RD1 | RD2 | RD3
             | CLB | CLC | IAC | CMC | CMA | RAL | RAR | TCC
             | DAC | TCS | STC | DAA | KBP | DCL

type reg     = R0  | R1  | R2  | R3  | R4  | R5  | R6  | R7
             | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15

type regpair = RP0 | RP1 | RP2 | RP3 | RP4 | RP5 | RP6 | RP7

type operand = Imm     of imm
             | Reg     of reg
             | RegPair of regpair

type ins = opcode * operand list

type 'a labled = { lbl: lable; global: bool; value: 'a }

type data = imm

type asm = { text_asm : (ins list labled) list ;
             data_asm : (data labled) list }

let string_of_lable (l: lable) : string = l

let string_of_imm : imm -> string = function
  | Lit i   -> Int32.to_string i
  | Lable l -> string_of_lable l

let string_of_reg : reg -> string = function
  | R0 -> "0"   | R1 -> "1"   | R2 -> "2"   | R3 -> "3"
  | R4 -> "4"   | R5 -> "5"   | R6 -> "6"   | R7 -> "7"
  | R8 -> "8"   | R9 -> "9"   | R10 -> "10" | R11 -> "11"
  | R12 -> "12" | R13 -> "13" | R14 -> "14" | R15 -> "15"

let string_of_regpair : regpair -> string = function
  | RP0 -> "0<" | RP1 -> "1<" | RP2 -> "2<" | RP3 -> "3<"
  | RP4 -> "4<" | RP5 -> "5<" | RP6 -> "6<" | RP7 -> "7<"

let string_of_opcode : opcode -> string = function
  | NOP -> "NOP" | JCN -> "JCN" | FIM -> "FIM" | SRC -> "SRC"
  | FIN -> "FIN" | JIN -> "JIN" | JUN -> "JUN" | JMS -> "JMS"
  | INC -> "INC" | ISZ -> "ISZ"
  | ADD -> "ADD" | SUB -> "SUB" | LD  -> "LD"  | XCH -> "XCH"
  | BBL -> "BBL" | LDM -> "LDM"
  | WRM -> "WRM" | WMP -> "WMP" | WRR -> "WRR" | WPM -> "WPM"
  | WR0 -> "WR0" | WR1 -> "WR1" | WR2 -> "WR2" | WR3 -> "WR3"
  | SBM -> "SBM" | RDM -> "RDM" | RDR -> "RDR" | ADM -> "ADM"
  | RD0 -> "RD0" | RD1 -> "RD1" | RD2 -> "RD2" | RD3 -> "RD3"
  | CLB -> "CLB" | CLC -> "CLC" | IAC -> "IAC" | CMC -> "CMC"
  | CMA -> "CMA" | RAL -> "RAL" | RAR -> "RAR" | TCC -> "TCC"
  | DAC -> "DAC" | TCS -> "TCS" | STC -> "STC" | DAA -> "DAA"
  | KBP -> "KBP" | DCL -> "DCL"

let string_of_operand : operand -> string = function
  | Imm i      -> string_of_imm i
  | Reg r      -> string_of_reg r
  | RegPair rp -> string_of_regpair rp

let rec string_of_operands (opnds: operand list) : string =
  begin match opnds with
    | []    -> ""
    | [h]   -> string_of_operand h
    | h::tl -> (string_of_operand h) ^ ", " ^ (string_of_operands tl)
  end

let string_of_ins (op, opnds: ins) : string =
  "\t" ^ (string_of_opcode op) ^ "\t" ^ (string_of_operands opnds)

let string_of_labled (f: 'a -> string) (l: 'a labled) : string = 
  string_of_lable l.lbl ^": \n" ^ f l.value

let string_of_asm (a: asm) : string =
  let map_concat_n f a = String.concat "\n" @@ List.map f a in
  map_concat_n (string_of_labled @@ map_concat_n string_of_ins) a.text_asm
  ^ "\n"

let reg_to_int : reg -> int32 = function
  | R0 -> 0l   | R1 -> 1l   | R2 -> 2l   | R3 -> 3l
  | R4 -> 4l   | R5 -> 5l   | R6 -> 6l   | R7 -> 7l
  | R8 -> 8l   | R9 -> 9l   | R10 -> 10l | R11 -> 11l
  | R12 -> 12l | R13 -> 13l | R14 -> 14l | R15 -> 15l

let regpair_to_int : regpair -> int32 = function
  | RP0 -> 0l | RP1 -> 1l | RP2 -> 2l | RP3 -> 3l
  | RP4 -> 4l | RP5 -> 5l | RP6 -> 6l | RP7 -> 7l

module Asm = struct
  let (~$) i      = (
    let v = Int32.of_int i in
      if v >= 16l || v < 0l then raise (Invalid_argument "invalid imm")
                            else Imm (Lit v)
  )
  let (~%) r      = Reg r
  let (~%%) rp    = RegPair rp
  let block  l is = { lbl = l; global = false; value = is }
  let gblock l is = { lbl = l; global = true;  value = is }
end

let prog0 = Asm.{
  text_asm = [ gblock "main"
    [ LDM, [~$4];
      XCH, [~%R1];
      LDM, [~$6];
      ADD, [~%R1];
      NOP, []
    ]
  ];
  data_asm = []
}

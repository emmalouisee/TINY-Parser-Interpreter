NOTE ERROR DIAGNOSTICS CAN ONLY BE DONE FOR VARIABLES x, y and z

This is closely based on Robert D. Cameron's code
 www.cs.sfu.ca/~cameron


1.  Syntactic and Semantic Domains of the variant of TINY

> import VariantParser

We had these definitions in VariantParser....

type Ide = String

data Exp = Zero | One | TT | FF |
           Read | I Ide | Not Exp |
           Equal Exp Exp | Plus Exp Exp
           deriving Show

data Cmd = Assign Ide Exp | Output Exp |
           IfThenElse Exp Cmd Cmd |
           WhileDo Exp Cmd |
           Seq Cmd Cmd
           deriving Show


Semantic Domains

> data Value = Numeric Integer | Boolean Bool | ERROR
>              deriving Show

> data MemVal = Stored Value | Unbound
>               deriving Show

> type Memory = ([Ide], Ide -> MemVal)

> type Input = [Value]

> type Output = [Value]

> type State = (Memory, Input, Output)



2.  Signatures of semantic functions.


> data ExpVal = OK Value State | Error

> data CmdVal = OKc State | Errorc

> exp_semantics :: Exp -> State -> ExpVal

> cmd_semantics :: Cmd -> State -> CmdVal
  
> display :: Memory -> String
> display ([], f) = "Memory: [] "
> display (l,f) = "Memory: " ++ concat [show(l !! x) ++ " = " ++ show(f (l !! x)) ++ " " | x <- [0..(length l)-1]]



3. Semantic Equations defining the semantic functions

> exp_semantics Zero s = OK (Numeric 0) s

> exp_semantics One s = OK (Numeric 1) s

> exp_semantics TT s = OK (Boolean True) s

> exp_semantics FF s = OK (Boolean False) s

> exp_semantics Read (m, [], o) = error (display m ++ "Input: " ++ "[] " ++ "Output: " ++ show o)

> exp_semantics Read (m, (i:is), o) = OK i (m, is, o)


> exp_semantics (I ident) ((l,f), i, o) =
>  case (f ident) of
>     Stored v  -> OK v ((l,f), i, o)
>     Unbound   -> error (display (l,f) ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)


> exp_semantics (Not exp) s =
>  case (exp_semantics exp s) of
>    OK (Boolean v) s1 -> OK (Boolean (not v)) s1
>    _ -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>         where (m,i,o) = s


> exp_semantics (Equal exp1 exp2) s =
>  case (exp_semantics exp1 s) of
>    OK (Numeric v1) s1 -> case (exp_semantics exp2 s1) of 
>                            OK (Numeric v2) s2 -> OK (Boolean (v1 == v2)) s2
>                            OK (Boolean v2) s2 -> OK (Boolean False) s2
>                            Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>                                     where (m,i,o) = s1
>    OK (Boolean v1) s1 -> case (exp_semantics exp2 s1) of 
>                            OK (Boolean v2) s2 -> OK (Boolean (v1 == v2)) s2
>                            OK (Numeric v2) s2 -> OK (Boolean False) s2
>                            Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>                                     where (m,i,o) = s1
>    Error -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>             where (m,i,o) = s


> exp_semantics (Plus exp1 exp2) s =
>  case (exp_semantics exp1 s) of
>    OK (Numeric v1) s1 -> case (exp_semantics exp2 s1) of
>                            OK (Numeric v2) s2 -> OK (Numeric (v1 + v2)) s2
>			     _ -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>                                 where (m,i,o) = s1
>    _ -> error (display m ++ "Input: " ++ show i ++ " " ++ "Output: " ++ show o)
>         where (m,i,o) = s


> update (l,f) ide val = ((ide:l), \ide2 -> if ide == ide2 then Stored val else f ide2)


> emptyMem ide = Unbound


> cmd_semantics (Assign ident exp) s =
>   case (exp_semantics exp s) of
>     OK v1 (m1, i1, o1) -> OKc (update m1 ident v1, i1, o1)
>     Error -> Errorc


> cmd_semantics (Output exp) s =
>   case (exp_semantics exp s) of
>     OK v1 (m1, i1, o1) -> OKc (m1, i1, o1 ++ [v1])
>     Error -> Errorc


> cmd_semantics (IfThenElse exp cmd1 cmd2) s =
>   case (exp_semantics exp s) of
>     OK (Boolean True) s1 -> cmd_semantics cmd1 s1
>     OK (Boolean False) s1 -> cmd_semantics cmd2 s1
>     _ -> Errorc


> cmd_semantics (WhileDo exp cmd) s =
>   case (exp_semantics exp s) of
>     OK (Boolean True) s1 -> cmd_semantics (Seq cmd (WhileDo exp cmd)) s1
>     OK (Boolean False) s1 -> OKc s1
>     _ -> Errorc


> cmd_semantics (Seq cmd1 cmd2) s =
>   case (cmd_semantics cmd1 s) of
>     OKc s1 -> cmd_semantics cmd2 s1
>     Errorc -> Errorc



 4.  Demo/Semantic Change/Demo

To demo the semantics in action, we use the following
"run" function to execute a TINY program for a given input.

> run program input =
>   case (cmd_semantics parsed_program (([], emptyMem), input, [])) of
>     OKc (m, i, o) -> o
>     Errorc -> [ERROR]
>   where parsed_program = cparse program


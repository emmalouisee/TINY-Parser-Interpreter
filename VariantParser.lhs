Grammar:

<cmd>    ::= <comp>;<cmd>|<comp>
<comp>   ::= if<expr>then<cmd>else<cmd>fi|repeat<cmd>until<expr>|(<cmd>)|
       	   <ide>becomes<expr>|print<expr>
<expr>   ::= <term>+<expr>|<erm>-<expr>|<term>=<expr>|<term>
<term>   ::= /<expr>|<factor>
<factor> ::= read|false|true|0|1|2|3|4|5|6|7|8|9|<ide>|(<expr>)

> import Parsing
>
> type Ide = String
>
> data Exp = Zero|One|Two|Three|Four|Five|Six|Seven|Eight|Nine|TT|FF|Read|
>      	     I Ide|Not Exp|Equal Exp Exp|Plus Exp Exp|Minus Exp Exp
>            deriving Show
>
> data Cmd = Assign Ide Exp|Output Exp|Cond Exp Cmd Cmd|RepeatUntil Cmd Exp|
>      	     Seq Cmd Cmd
>            deriving Show
>
>
> cmd :: Parser Cmd
> cmd          = do c1 <- comp
>	            symbol ";"
>		    c2 <- cmd
>		    return (Seq c1 c2)
>		 +++
>		 comp
>
>
> comp :: Parser Cmd
> comp          = do symbol "if"
>	       	     c1 <- expr
>		     symbol "then"
>                    c2 <- cmd
>		     symbol "else"
>		     c3 <- cmd
>		     symbol "fi"
>		     return (Cond c1 c2 c3)
>		  +++
>		  do symbol "repeat"
>		     c1 <- cmd
>		     symbol "until"
>		     c2 <- expr
>		     return (RepeatUntil c1 c2)
>		  +++
>		  do symbol "("
>		     c1 <- cmd
>		     symbol ")"
>		     return c1
>                 +++
>		  do symbol "print"
>		     c1 <- expr
> 		     return (Output c1)
>		  +++
>		  do c1 <- token isIdentifier
>		     symbol "becomes"
>		     c2 <- expr
>		     return (Assign c1 c2)
>
>
> expr :: Parser Exp
> expr		= do e1 <- term
>		     symbol "+"
>		     e2 <- expr
>		     return (Plus e1 e2)
>		  +++
>		  do e1 <- term
>		     symbol "-"
>		     e2 <- expr
>		     return (Minus e1 e2)
>		  +++
>		  do e1 <- term
>		     symbol "="
>		     e2 <- expr
>		     return (Equal e1 e2)
>		  +++
>		  term
>
>
> term :: Parser Exp
> term		= do symbol "/"
>		     e1 <- expr
>		     return (Not e1)
>                 +++
>		  factor
>
>
> factor :: Parser Exp
> factor	  =
>		    do e1 <- token isIdentifier
>		       return (I e1)
>		    +++
>	            do symbol "read"
>		       return Read
>		    +++
>		    do symbol "false"
>		       return FF
>		    +++
>		    do symbol "true"
>		       return TT
>		    +++
>		    do symbol "0"
>		       return Zero
>		    +++
>		    do symbol "1"
>		       return One
>		    +++
>		    do symbol "2"
>		       return Two
>		    +++
>		    do symbol "3"
>		       return Three
>		    +++
>		    do symbol "4"
>		       return Four
>		    +++
>		    do symbol "5"
>		       return Five
>		    +++
>		    do symbol "6"
>		       return Six
>		    +++
>		    do symbol "7"
>		       return Seven
>		    +++
>		    do symbol "8"
>		       return Eight
>		    +++
>		    do symbol "9"
>		       return Nine
>		    +++
>		    do symbol "("
>		       e1 <- expr
>		       symbol ")"
>		       return e1
>
>
> isIdentifier   :: Parser String
> isIdentifier            = do c <- lower
> 		      	       cs <- many alphanum
> 		      	       if ((c:cs) == "true" || (c:cs) == "false" || (c:cs) == "read")
>			       then mempty
>			       else return (c:cs)


A function to try the command parser with:

> eval           :: String -> Cmd
> eval xs        =  case (parse cmd xs) of
>                         [(n,[])]  -> n
>                         [(_,out)] -> error ("unused input " ++ out)
>                         []        -> error "invalid input"

# Cotton

LLVMで動く試作言語

```
==========
source 

def fact(n: Int): Int {
    if (n == 0) {
        1
    } else {
        n * fact(n-1)
    }
}

==========
lexer

[Def (Pos {pos = AlexPn 0 1 1}),Lower (StrP {text = "fact", pos = AlexPn 4 1 5}),LParen (Pos {pos = AlexPn 8 1 9}),Lower (StrP {text = "n", pos = AlexPn 9 1 10}),Colon (Pos {pos = AlexPn 10 1 11}),Upper (StrP {text = "Int", pos = AlexPn 12 1 13}),RParen (Pos {pos = AlexPn 15 1 16}),Colon (Pos {pos = AlexPn 16 1 17}),Upper (StrP {text = "Int", pos = AlexPn 18 1 19}),LBrace (Pos {pos = AlexPn 22 1 23}),If (Pos {pos = AlexPn 28 2 5}),LParen (Pos {pos = AlexPn 31 2 8}),Lower (StrP {text = "n", pos = AlexPn 32 2 9}),Op (StrP {text = "==", pos = AlexPn 34 2 11}),Num (NumP {num = 0, pos = AlexPn 37 2 14}),RParen (Pos {pos = AlexPn 38 2 15}),LBrace (Pos {pos = AlexPn 40 2 17}),Num (NumP {num = 1, pos = AlexPn 50 3 9}),RBrace (Pos {pos = AlexPn 56 4 5}),Else (Pos {pos = AlexPn 58 4 7}),LBrace (Pos {pos = AlexPn 63 4 12}),Lower (StrP {text = "n", pos = AlexPn 73 5 9}),Op (StrP {text = "*", pos = AlexPn 75 5 11}),Lower (StrP {text = "fact", pos = AlexPn 77 5 13}),LParen (Pos {pos = AlexPn 81 5 17}),Lower (StrP {text = "n", pos = AlexPn 82 5 18}),Op (StrP {text = "-", pos = AlexPn 83 5 19}),Num (NumP {num = 1, pos = AlexPn 84 5 20}),RParen (Pos {pos = AlexPn 85 5 21}),RBrace (Pos {pos = AlexPn 91 6 5}),RBrace (Pos {pos = AlexPn 93 7 1})]

==========
構文解析

def fact(n: Int): Int {
	if n == 0 {
		1
	} else {
		n * fact(n - 1)
	}
}

==========
Alpha変換

def fact(fact#n: Int): Int {
	if fact#n == 0 {
		1
	} else {
		fact#n * fact(fact#n - 1)
	}
}

==========
型検査

Env {_typeOf = fromList [("*",(Int, Int): Int),("+",(Int, Int): Int),("-",(Int, Int): Int),("/",(Int, Int): Int),("==",(Int, Int): Bool),("fact",(Int): Int),("fact#n",Int)]}

==========
暗黙引数検査

ImplicitArgs {_implicitArgs = fromList []}

==========
Clojure変換

def fact(fact#n: Int): Int {
	if fact#n == 0 {
		1
	} else {
		fact#n * fact(fact#n - 1)
	}
}

==========
K正規化

def fact(fact#n): Int {
	VZdjw3_P = fact#n
	Rbnd6HD1 = 0
	#return = VZdjw3_P == Rbnd6HD1
	if ( #return ) {
		#return = 1
	} else {
		amWFX22J = fact#n
		3lSzadk5 = fact#n
		2Hpb4ObH = 1
		AYZZVUBm = 3lSzadk5 - 2Hpb4ObH
		SIKi7CsK = fact(AYZZVUBm)
		#return = amWFX22J * SIKi7CsK
	}
}
```


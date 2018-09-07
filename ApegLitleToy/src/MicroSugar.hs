module MicroSugar where

import APEGInterp
import Control.Monad.State.Lazy
import AbstractSyntax
import APEGState
import APEGTypeSystem

alts :: [APeg] -> APeg
alts  = foldr1 Alt

seqs :: [APeg] -> APeg
seqs = foldr1 Seq

chrs :: [Char] -> APeg
chrs = alts.(map (Lit.(:[])))

many1 :: APeg -> APeg
many1 p = Seq p (Kle p)  

digit :: APeg
digit = chrs ['0'..'9']

num :: APeg
num = Kle digit

upper :: APeg
upper = chrs ['A'..'Z']

lower :: APeg
lower = chrs ['a'..'z']

identifier :: APeg
identifier = Seq lower (Kle (Alt lower digit ))

reswrd :: String -> APeg
reswrd s = Lit s

fator :: APeg
fator = Alt num identifier

wht :: APeg 
wht = chrs ['\n',' ','\t']

whts :: APeg 
whts = Kle wht

whts1 :: APeg 
whts1 = many1 wht


g :: Expr
g = EVar "g"
-- ========== MicroSugar APEG Grammar ============= --


micro :: ApegGrm
micro = [ruleBlock,ruleStmt,ruleExpr,ruleCExpr,ruleFator]


ruleProg :: ApegRule
ruleProg = ApegRule "prog" 
                    [(TyLanguage,"g")]
                    []
                    (Kle (NT "newSyn" [g] ["sigma"]))
--                     (Seq (Kle (NT "newSyn" [g] ["sigma"]))
--                          (many1 (NT "extStmt" [g,EVar "sigma"] [])))

ruleNewSyn :: ApegRule
ruleNewSyn = ApegRule "newSyn"
                      [(TyLanguage,"g")]
                      [(TyLanguage,EVar "s")]
                      (seqs [AEAttr [("s",Epsilon)],
                             reswrd "define",
                             wht,
                             Bind "n" identifier,
                             whts,
                             Lit "{",
                             whts,
                             Kle $ seqs [NT "rule" [g] ["r"],
                                         AEAttr [("s", Union (EVar "s") (EVar "r"))]],
                             whts,
                             Lit "}"
                             ])
                                
ruleRule :: ApegRule
ruleRule = ApegRule "rule"
                    [(TyLanguage,"g")] -- Tem um problema aqui ! A syntaxe não tem meios para falar de tipos !
                    [(TyLanguage,MkRule (Str "ru") [(TyLanguage,"g")] [] (MetaPeg MkLambda))]
                    (Lit ";")

ruleExtStmt :: ApegRule
ruleExtStmt = ApegRule "rStmt"
                       [(TyLanguage,"g")]
                       []
                       Lambda
                       
ruleBlock :: ApegRule 
ruleBlock = ApegRule "block"
                     [(TyLanguage,"g")]
                     []
                     (seqs [ whts,
                            Lit "{",
                            many1 (Seq whts (NT "stmt" [g] [] ) ),
                            whts,
                            Lit "}"])
                     
ruleStmt :: ApegRule 
ruleStmt = ApegRule "stmt"
                     [(TyLanguage,"g")]
                     []
                     ( alts [seqs [Lit "print(", whts, NT "expr" [g] [], whts, Lit ")", whts, (Lit ";")],
                             seqs [Lit "read(", whts, identifier , whts, Lit ")", whts, (Lit ";")],
                             seqs [Lit "if(",whts, NT "cexpr" [g] [] ,whts,Lit ")", NT "block" [g] []],
                             seqs [Lit "loop(",whts, NT "cexpr" [g] [] ,whts,Lit ")", NT "block" [g] []],
                             seqs [identifier, whts, Lit ":=" ,whts, NT "expr" [g] [] , whts, Lit ";"]
                            ])

ruleExpr :: ApegRule 
ruleExpr = ApegRule "expr"
                    [(TyLanguage,"g")]
                    []
                    (seqs [ NT "cexpr" [g] [],whts, Kle $ seqs [chrs ['+','-'], whts,  NT "cexpr" [g] []]])
                    
ruleCExpr :: ApegRule 
ruleCExpr = ApegRule "cexpr"
                     [(TyLanguage,"g")]
                     []
                     (seqs [ NT "fator" [g] [], whts, Kle $ seqs [chrs ['<','='],whts,  NT "fator" [g] [] ]])
                        
ruleFator :: ApegRule 
ruleFator = ApegRule "fator"
                     [(TyLanguage,"g")]
                     []
                     (alts [ seqs [Lit "(", whts, NT "expr" [g] [], whts, Lit ")"],
                             Lit "true",
                             Lit "false",
                             num,    
                             identifier
                             ])
                             

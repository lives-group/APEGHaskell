module MicroSugar where

import APEG.Interpreter.APEGInterp
import APEG.Interpreter.MonadicState
import APEG.Interpreter.MaybeString
import APEG.Interpreter.State
import Control.Monad.State.Lazy
import APEG.AbstractSyntax
import APEG.Interpreter.State
import APEG.TypeSystem

{-  DSL for representing the grammar
       rebindable syntaxe
       Special mkar to eliminate our up some nonTerminals
-}

alts :: [APeg] -> APeg
alts  = foldl1 Alt

seqs :: [APeg] -> APeg
seqs = foldl1 Seq

chrs :: [Char] -> APeg
chrs = alts.(map (Lit.(:[])))

many1 :: APeg -> APeg
many1 p = Seq p (Kle p)  

digit :: APeg
digit = chrs ['0'..'9']

num :: APeg
num = many1 digit

upper :: APeg
upper = chrs ['A'..'Z']

lower :: APeg
lower = chrs ['a'..'z']

lit :: APeg
lit = Kle $ chrs "()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"

identifier :: APeg
identifier = Seq lower (Kle (Alt lower digit ))

rid :: APeg
rid = Seq (Alt lower upper) (Kle $ (alts [lower,upper,digit]))

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


microSugar :: ApegGrm
microSugar = [ruleProg,ruleNewSyn,ruleRule,rulePattern,ruleSeq,ruleExtStmt,ruleKFator,rulepFator,ruleExpr,ruleBlock,ruleStmt,ruleCExpr,ruleFator,whiteRule]


ruleProg :: ApegRule
ruleProg = ApegRule "prog" 
                    [(TyLanguage,"g")]
                    []
                    (seqs [ AEAttr [("sigma",MapLit [(Str "0",Epsilon)])],
                            Kle (NT "newSyn" [g] ["sigma"]),
                            many1 $ Alt (NT "rStmt" [g,EVar "sigma"] []) (NT "block" [g] []) 
                          ])

ruleNewSyn :: ApegRule
ruleNewSyn = ApegRule "newSyn"
                      [(TyLanguage,"g")]
                      [(TyMap TyGrammar,EVar "s")]
                      (seqs [AEAttr [("lan",Epsilon)],
                             Lit "define",
                             wht,
                             Bind "n" rid,
                             whts,
                             Lit "{",
                             whts,
                             Kle $ seqs [NT "rule" [g] ["r"],
                                         AEAttr [("lan", Union (EVar "lan") (EVar "r"))]],
                             whts,
                             Lit "}",
                             AEAttr [("s",MapLit [(EVar "n",EVar "lan")])] 
                            ])
                                
ruleRule :: ApegRule
ruleRule = ApegRule "rule"
                    [(TyLanguage,"g")] -- Tem um problema aqui ! A sintaxe não tem meios para falar de tipos !
                    [(TyGrammar,MkRule (EVar "nt") [(MetaExp MkTyLanguage,Str "g")] [] (EVar "p"))]
                    (seqs [ whts,
                            Bind "nt" rid,
                            whts,
                            Lit "->",
                            whts,
                            NT "pattern" [g] ["p"],
                            whts,
                            Lit ";"
                           ] )

rulePattern :: ApegRule 
rulePattern = ApegRule "pattern"
                        [(TyLanguage,"g")]
                        [(TyMetaAPeg,EVar "pe")]
                        (seqs [ NT "pseq" [g] ["pe"],
                                Kle (seqs [whts,
                                           Lit "/", 
                                           whts, 
                                           NT "pseq" [g] ["pd"],
                                           AEAttr [("pe",MetaPeg $ MkAlt (EVar "pe") (EVar "pd"))]])
                                
                              ])
--                               
ruleSeq :: ApegRule 
ruleSeq = ApegRule "pseq"
                        [(TyLanguage,"g")]
                        [(TyMetaAPeg,EVar "pe")]
                        (seqs [NT "pKFator" [g] ["pe"],  
                               Kle $ seqs [
                                           whts,
                                           NT "pKFator" [g] ["pd"],
                                           AEAttr [("pe",MetaPeg $ MkSeq (EVar "pe") (EVar "pd"))]
                              ]])

ruleKFator :: ApegRule 
ruleKFator = ApegRule "pKFator" 
                     [(TyLanguage, "g")]
                     [(TyMetaAPeg,  EVar "pf")]
                     (seqs [ NT "pFator" [g] ["kf"], 
                             whts, 
                             alts [ Seq (Lit "*") (AEAttr[("pf", MetaPeg $ MkKle (EVar "kf"))]),
                                    Seq (Lambda)  (AEAttr[("pf", EVar "kf")])]])
                            
                           
rulepFator :: ApegRule 
rulepFator = ApegRule "pFator" 
                     [(TyLanguage, "g")]
                     [(TyMetaAPeg,  EVar "pf")]
                     (alts [seqs [ Lit "!", 
                                   whts, 
                                   NT "pFator" [g] ["f"], 
                                   AEAttr[("pf",MetaPeg $ MkNot (EVar "f"))] ],
                            seqs [ Lit "\"", 
                                  Bind "f" lit, 
                                  Lit "\"", 
                                  AEAttr[("pf",MetaPeg $ MkLit (EVar "f"))]],
                            seqs [ Lit "(",
                                   whts,
                                   NT "pattern" [g] ["pf"],
                                   whts,
                                   Lit ")"],
                            seqs [Bind "ntName" rid,
                                  AEAttr[("pf",MetaPeg $ MkCal (EVar "ntName") [MetaExp (MVar $ Str "g")] [])] ]
                                   
                           ])

ruleExtStmt :: ApegRule
ruleExtStmt = ApegRule "rStmt"
                       [(TyLanguage,"g"),(TyMap TyGrammar,"sigma")]
                       []
                       (seqs [ whts,
                               Lit "syntax",
                               AEAttr [("nwSyn",Epsilon)],
                               whts,
                               (Bind "n" rid),
                               AEAttr [("nwSyn",Union (MapAccess (EVar "sigma") (EVar "n")) (EVar "nwSyn"))],
                               Kle (seqs [whts,
                                          Lit ",", 
                                          whts,
                                          Bind "n" rid,
                                          AEAttr [("nwSyn",Union (MapAccess (EVar "sigma") (EVar "n")) (EVar "nwSyn"))]
                                         ]),
                               whts,
                               NT "block" [Union g (EVar "nwSyn")] []
                               ] )


ruleBlock :: ApegRule 
ruleBlock = ApegRule "block"
                     [(TyLanguage,"g")]
                     []
                     (seqs [whts,
                            Lit "{",
                            many1 (NT "stmt" [g] []),
                            whts,
                            Lit "}"])
                     
ruleStmt :: ApegRule 
ruleStmt = ApegRule "stmt"
                     [(TyLanguage,"g")]
                     []
                     ( Seq whts
                           (alts [seqs [Lit "print(", NT "expr" [g] [], whts, Lit ")", whts, (Lit ";")],
                                  seqs [Lit "read(", whts, identifier , whts, Lit ")", whts, (Lit ";")],
                                  seqs [Lit "if(", NT "cexpr" [g] [] ,whts,Lit ")", whts ,NT "block" [g] []],
                                  seqs [Lit "loop(", NT "cexpr" [g] [] ,whts, Lit ")", whts ,NT "block" [g] []],
                                  seqs [identifier, whts, Lit ":=" ,whts, NT "expr" [g] [] , whts, Lit ";"]
                                ]))

ruleExpr :: ApegRule 
ruleExpr = ApegRule "expr"
                    [(TyLanguage,"g")]
                    []
                    (seqs [NT "cexpr" [g] [],whts, Kle $ seqs [chrs ['+','-'], whts,  NT "cexpr" [g] []]])
                    
ruleCExpr :: ApegRule 
ruleCExpr = ApegRule "cexpr"
                     [(TyLanguage,"g")]
                     []
                     (seqs [NT "fator" [g] [], whts, Kle $ seqs [chrs ['<','='],whts,  NT "fator" [g] [] ]])
                        
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
                           
whiteRule :: ApegRule 
whiteRule = ApegRule "whites"
                     [(TyLanguage, "g")]
                     []
                     (whts)
                           
runMS :: FilePath -> IO (VEnv,MybStr,String,Result)
runMS ph = do f <- readFile ph
              return $ simpleTestWithArgs microSugar [] f
              


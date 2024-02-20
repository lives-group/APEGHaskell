module GrammarGrammar where

import APEG.Interpreter.APEGInterp
import APEG.Interpreter.MonadicState
import APEG.Interpreter.MaybeString
import APEG.Interpreter.State
import Control.Monad.State.Lazy
import APEG.AbstractSyntax
import APEG.Interpreter.State
import APEG.TypeSystem

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
-- ========== Grammar Grammar ============= --


-- pattern<g,me> -> Seq<g,pe> ('/' Seq<g,pd> {pe =  pe #/ pd} )*
pattern :: ApegRule 
pattern = ApegRule "pattern"
                [(TyLanguage,"g")]
                [(TyMetaAPeg,EVar "pe")]
                (seqs [ NT "pseq" [g] ["pe"],
                        Kle (seqs [whts,
                                   Lit "/", 
                                   whts, 
                                   NT "pseq" [g] ["pd"],
                                   Update [("pe",MetaPeg $ MkAlt (EVar "pe") (EVar "pd"))]])
                                
                              ])
-- pseq<g,pe> -> pKFator<g,pe> ( pKfator<g,pd> { pe = pe #. pd})                             
pseq :: ApegRule 
pseq = ApegRule "pseq"
                 [(TyLanguage,"g")]
                 [(TyMetaAPeg,EVar "pe")]
                 (seqs [NT "pKFator" [g] ["pe"],  
                        Kle $ seqs [ whts,
                                     NT "pKFator" [g] ["pd"],
                                     Update [("pe",MetaPeg $ MkSeq (EVar "pe") (EVar "pd"))]
                                   ]])
                                   
--pKfactor<g,pf> -> pFator<g,kf> ('*' { pf = kf #* } / {pf = kf} )  
pKFator :: ApegRule 
pKFator = ApegRule "pKFator" 
                   [(TyLanguage, "g")]
                   [(TyMetaAPeg,  EVar "pf")]
                   (seqs [ NT "pFator" [g] ["kf"], 
                           whts, 
                           alts [ Seq (Lit "*") (Update[("pf", MetaPeg $ MkKle (EVar "kf"))]),
                                  Seq (Lambda)  (Update[("pf", EVar "kf")])]])
                            
{-
pFator<g,pf> -> '!' pFator<g,f> {pf = #! f}
             / '\"' f <- LIT '\"' {pf = #Lit f}
             / '(' pattern<g,pf> ')'
             / ntName <- ID { pf = #call ntName <#MVar #g>}
             
-}
pFator :: ApegRule 
pFator = ApegRule "pFator" 
                  [(TyLanguage, "g")]
                  [(TyMetaAPeg,  EVar "pf")]
                  (alts [seqs [ Lit "!", 
                                whts, 
                                NT "pFator" [g] ["f"], 
                                Update[("pf",MetaPeg $ MkNot (EVar "f"))] ],
                          seqs [ Lit "\"", 
                                 Bind "f" lit, 
                                 Lit "\"", 
                                 Update[("pf",MetaPeg $ MkLit (EVar "f"))]],
                          seqs [ Lit "(",
                                 whts,
                                 NT "pattern" [g] ["pf"],
                                 whts,
                                 Lit ")"],
                          seqs [Bind "ntName" rid,
                                Update[("pf",MetaPeg $ MkCal (EVar "ntName") [MetaExp (MVar $ Str "g")] [])] ]
                           ])


example = [pattern,pseq, pKFator,pFator ]

runMS :: FilePath -> IO (VEnv,MybStr,String,Result)
runMS ph = do f <- readFile ph
              return $ simpleTestWithArgs example [] f
              


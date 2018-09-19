{-|
Module      : APEG.Utils.TraceString
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : (c) Leonardo Vieira dos Santos Reis, 2018
                  Rodrigo Geraldo Ribeiro, 2018
                  Elton M. Cardoso, 2018
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module defines the type TraceString which is a string that can have no value at all. 
Operations on a TraceString that has no value, (a Null String), will result in a String,
that has no Value. The utility for this string is to save memory on computation of the value
of a APEG bind operation. 

-}

module APEG.Utils.TraceString (
    TraceStr,
    rVal,
    rErr,
    (?:),
    nullTraceStr,
    emptyTraceStr,
    fromTraceStr
    )where


-- | A TraceStr is a String that may be initialized with an actual string, or may not be a null String. 
-- Any operation on null String will result a null String.

data TraceStr = NullStr | Str String  deriving (Show,Eq,Ord)

-- | Cons operation for TraceStr
(?:) :: Char ->TraceStr -> TraceStr
(?:) _ NullStr = NullStr
(?:) x (Str xs) = Str (x:xs) 

-- | Concat operation for TraceStr
(?++) :: TraceStr -> TraceStr -> TraceStr 
(?++) NullStr   xs       = xs
(?++) ys        NullStr  = ys
(?++) (Str xs) (Str ys)  = (Just $ xs ++ ys)

-- | Null constante for TraceStr. Note that the empty String is difernete form
-- from the null String.
nullTraceStr :: TraceStr
nullTraceStr = NullStr

-- | Empty TraceStr. 
emptyTraceStr :: TraceStr
emptyTraceStr = Str []

-- | Converting TraceString to String. Returns "" on null TraceStr
fromTraceStr :: TraceStr -> String
fromTraceStr Null = ""
fromTraceStr (Str xs) = xs

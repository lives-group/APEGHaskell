{-|
Module      : APEG.Interpreter.MaybeString
Description : Representation of a state for the APEG interpreter/type-checker
Copyright   : 
License     : GPL-3
Stability   : experimental
Portability : POSIX

This module defines the type MaybeString which is a string that can have no value at all. 
Operations on a MaybeString that has no value, (a Null String), will result in a String,
that has no Value. The utility for this string is to save memory on computation of the value
of a APEG bind operation. 

-}

module APEG.Interpreter.MaybeString (
    MybStr,
    (?:),
    (?++),
    (>++),
    (++>),
    nullMybStr,
    emptyMybStr,
    fromMybStr,
    mbStr
)where


-- | A MybStr is a String that may be initialized with an actual string, or may not be a null String. 
-- Any operation on null String will result a null String.

data MybStr = NullStr | Str String  deriving (Show,Eq,Ord)

-- | Cons operation for MybStr
(?:) :: Char ->MybStr -> MybStr
(?:) _ NullStr = NullStr
(?:) x (Str xs) = Str (x:xs) 

-- | Concat operation for MybStr
(?++) :: MybStr -> MybStr -> MybStr 
(?++) NullStr  xs       = xs
(?++) ys       NullStr  = ys
(?++) (Str xs) (Str ys) = (Str $ xs ++ ys)

-- | Insert a string to the beginning of a MybStr
(>++) :: String -> MybStr -> MybStr
(>++) [] xs       = xs
(>++) xs NullStr  = Str xs
(>++) xs (Str ys) = (Str $ xs ++ ys)

-- | Insert a string to the end of a MybStr
(++>) :: MybStr -> String -> MybStr
(++>) NullStr   xs       = Str xs
(++>) (Str xs) ys  = (Str $ xs ++ ys)

-- | Null constante for MybStr. Note that the empty String is difernete form
-- from the null String.
nullMybStr :: MybStr
nullMybStr = NullStr

-- | Empty MybStr. 
emptyMybStr :: MybStr
emptyMybStr = Str []

-- | Converting MybString to String. Returns "" on null MybStr
fromMybStr :: MybStr -> String
fromMybStr NullStr = ""
fromMybStr (Str xs) = xs

-- | Convert a MybStr to MaybeString
mbStr :: MybStr -> Maybe String
mbStr NullStr = Nothing
mbStr (Str x) = Just x


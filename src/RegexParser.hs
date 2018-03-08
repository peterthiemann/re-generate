module RegexParser (parseRe, pretty) where

import qualified Data.List as L
import qualified Data.Set as Set
import ParserCon
import Data.Char (isDigit)
import GRegexp (GRE(..))

-- ^ Smart constructors

-- mkSet = Set . Set.fromList
-- allChar = Set.fromList $ enumFromTo ' ' '~' -- Only printables. Does not contain \n.
-- anyChar = Set allChar

-- ^ List versions of the binary operators.

listOp binOp l =
    case l of
      [x] -> x
      (c:l) -> L.foldl' binOp c l

altL, seqL, unionL :: [GRE t] -> GRE t
altL = listOp Or
seqL = listOp Dot
unionL = listOp And

-- Pretty printing for regular expressions
pretty (Atom c) = [c]
-- pretty r | r == anyChar = "."
-- pretty (Set s) =
--     case Set.elems s of
--       [] -> pretty Void
--       [c] -> if L.elem c mustEscape then ['\\',c] else [c]
--       l -> "[" ++ l ++ "]"
pretty (Or r r') = "(" ++ pretty r ++ "|" ++ pretty r' ++ ")"
pretty (Dot r r') = pretty r ++ pretty r'
pretty (And r r') = pretty r ++ "&" ++ pretty r'
pretty One = ""
pretty Zero = "[]"
pretty (Star r) = "(" ++ pretty r ++ ")*"
pretty (Not r) = "~(" ++ pretty r ++ ")"
-- pretty (Rep i j r) =  "(" ++ pretty r ++ ")" ++ prettyRep i j
--     where prettyRep 0 Nothing = "*"
--           prettyRep 1 Nothing = "+"
--           prettyRep i Nothing = "{" ++ pretty i ++ ",}"
--           prettyRep i (Just j) = "{" ++ pretty i ++ "," ++ pretty j ++ "}"


-- ^ Parsing

parseRe :: String -> Maybe (GRE Char)
parseRe s = lexer s >>= parse p_exp1

p_exp1, p_exp2, p_exp3, p_exp4 :: Parser Tok (GRE Char)
p_exp1 = altL <$> p_alt
p_alt = ((:) <$> p_exp2 <*> many (lit TAlt *> p_exp2))
p_exp2 = seqL <$> p_seq
p_seq = some p_exp3
p_exp3 = unionL <$> p_union
p_union = ((:) <$> p_exp4 <*> many (lit TAnd *> p_exp4))

p_exp4 =
    flip ($) <$> p_atom <*> p_rep
    <|> lit TNot *> (Not <$> p_atom)
p_rep = Star <$ lit TStar
    -- <|> Plus <$ lit TPlus
    -- <|> onRep (uncurry rep)
    <|> pure id
p_atom = onChar Atom
     <|> lit TLParen *> p_exp1 <* lit TRParen
     -- <|> onSet (\(b,c) -> (if b then id else mkCompl) $ mkSet c)
     -- <|> anyChar <$ lit TDot

on test f = do
   t <- satisfy (const True)
   case test t of
     Nothing -> empty
     Just x -> return $ f x

onChar = on $ \s -> case s of {TChar x -> Just x ; _ -> Nothing}
onSet = on $ \s -> case s of {TSet b x -> Just (b,x) ; _ -> Nothing}
onRep = on $ \s -> case s of {TRep i j -> Just (i,j) ; _ -> Nothing}




-- ^ Lexing

data Tok = TChar Char | TStar
         | TPlus | TAlt | TAnd
         | TRParen | TLParen
         | TSet Bool [Char] | TRep Int (Maybe Int)
         | TDot
         | TNot
           deriving (Show,Eq)

lexer :: String -> Maybe [Tok]
lexer = parse $ many t_tok

t_tok = t_symb <|> t_set <|> t_rep <|> t_atom

t_char is_escaped =
        lit '\\' *> satisfy is_escaped
    <|> satisfy (not . is_escaped)

t_atom =  fmap TChar $ t_char $ \c -> L.elem c mustEscape
t_set_elem = t_char (==']')
t_set = TSet
        <$> (lit '[' *> ((== Nothing) <$> optional (lit '^')))
        <*> (many t_set_elem <* lit ']')
t_rep = TRep <$> (lit '{' *> t_int) <*> (lit ',' *> optional t_int <* lit '}')
t_int = read <$> some (satisfy isDigit)

t_symb =
        TStar <$ lit '*'
    <|> TPlus <$ lit '+'
    <|> TAlt <$ lit '|'
    <|> TAnd <$ lit '&'
    <|> TDot <$ lit '.'
    <|> TLParen <$ lit '('
    <|> TRParen <$ lit ')'
    <|> TNot <$ lit '~'

mustEscape = "\\()*+[.|&{~"

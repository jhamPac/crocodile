module Parser where

import           Control.Applicative   hiding ((<|>))
import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           LispVal
import           Text.Parsec
import           Text.Parsec.Expr
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
    Tok.commentStart = "{-"
    , Tok.commentEnd = "-}"
    , Tok.commentLine = "--"
    , Tok.opStart = Tok.opLetter style
    , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
    , Tok.identStart = letter <|> oneOf "-+/*|&><"
    , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
    , Tok.reservedOpNames = [ "'", "\""]
}

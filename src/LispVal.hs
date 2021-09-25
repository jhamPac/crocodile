{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Exception
import           Control.Monad.Reader
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool deriving (Typeable)

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
    deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

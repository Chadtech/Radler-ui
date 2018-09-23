{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}


module Program 
    ( Program
    , init
    , run
    , set
    , getModel
    , mapModel
    ) where

      
import Control.Applicative
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Reader as CMR
import Data.Text.Lazy (Text)
import Model (Model)
import Prelude hiding (init)
import Web.Scotty.Trans (ScottyT)
import qualified Web.Scotty.Trans as Web


newtype Program a 
    = Program 
        { run :: CMR.ReaderT (STM.TVar Model) IO a }
            deriving 
                ( Applicative
                , Functor
                , Monad
                , CMR.MonadIO
                , CMR.MonadReader (STM.TVar Model)
                )


init :: Model -> ScottyT Text Program () -> IO ()
init initialModel router =
    STM.newTVarIO initialModel
        >>= (initScotty router)


initScotty :: ScottyT Text Program () -> STM.TVar Model -> IO ()
initScotty router modelMemory =
    Web.scottyT 3000 (runActionToIO modelMemory) router


runActionToIO :: STM.TVar Model -> Program a -> IO a
runActionToIO modelMemory app = 
    CMR.runReaderT (run app) modelMemory


set :: CMR.MonadTrans t => Program a -> t Program a
set = 
    CMR.lift


getModel :: (Model -> b) -> Program b
getModel f = 
    CMR.ask 
        >>= CMR.liftIO . STM.readTVarIO 
        >>= return . f


mapModel :: (Model -> Model) -> Program ()
mapModel f = 
    CMR.ask 
        >>= CMR.liftIO . STM.atomically . flip STM.modifyTVar' f

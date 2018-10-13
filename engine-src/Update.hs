{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

import Data.Response (Response)
import qualified Data.Response as Response
import Data.Route (Route(..))
import qualified Data.Route as Route
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Error (Error)
import qualified Error
import Flow
import Json (Json)
import qualified Json
import Msg (Msg(..))
import Model (Model)
import qualified Model
import qualified Prelude.Extra as PE
import Result (Result(Err, Ok))
import qualified Result


update :: Msg -> Model -> ( Model, Response )
update msg model =
    case msg of
        Request Nothing ->
            ( model, Response._404)

        Request (Just route) ->
            handleRoute route model


handleRoute :: Route -> Model -> ( Model, Response )
handleRoute route model =
    case route of
        Ping ->
            ( model, Response.ping )

        Echo body ->
            ( model, Response.text body )

        Play (Ok score) ->
            ( Model.setScore score model
            , playResponse Nothing
            )

        Play (Err err) ->
            ( model
            , playResponse (Just err)
            )


playResponse :: Maybe Error -> Response
playResponse 
    = Response.json
    . Json.maybe
    . fmap (Json.string . Error.throw)


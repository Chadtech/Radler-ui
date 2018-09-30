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
import Prelude.Extra ((<<))
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

        Init (Ok score) ->
            ( Model.setScore score model
            , initResponse Nothing
            )

        Init (Err err) ->
            ( model
            , initResponse (Just err)
            )


initResponse :: Maybe Error -> Response
initResponse maybeError =
    [ ("type", Json.string "Init") 
    , maybeError
        |> PE.mapMaybe (Json.string << Error.throw)
        |> Json.maybe 
        |> (,) "payload"
    ]
      |> Json.object
      |> Response.json


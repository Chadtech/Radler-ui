{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

import qualified Audio
import Cmd (Cmd)
import qualified Cmd
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Text.Lazy as T
import Error (Error)
import qualified Error
import Json (Json)
import qualified Json
import Msg (Msg(..))
import Model (Model)
import qualified Model
import Response (Response)
import qualified Response
import Result (Result(Err, Ok))
import qualified Result
import Route (Route(..))
import qualified Route
import Score (Score)
import qualified Score


update :: Msg -> Model -> ( Model, Cmd, Response )
update msg model =
    case msg of
        Request Nothing ->
            ( model
            , Cmd.none
            , Response.error
                404
                "end point unsupported"
            )

        Request (Just route) ->
            handleRoute route model


handleRoute :: Route -> Model -> ( Model, Cmd, Response )
handleRoute route model =
    case route of
        Ping ->
            ( model
            , Cmd.none
            , Response.ping 
            )

        Echo body ->
            ( model
            , Cmd.none
            , Response.text body 
            )

        Play (Ok score) ->
            ( Model.setScore score model
            , playScore score model
            , Response.json Json.null
            )

        Play (Err err) ->
            ( model
            , Cmd.none
            , Response.json $ Json.object
                [ (,) "error"
                    $ Json.string 
                    $ Error.throw
                    $ err
                ]
            )


playScore :: Score -> Model -> Cmd
playScore incomingScore model =
    case Model.score model of
        Just existingScore ->
            Cmd.log "Score exists"
            
        Nothing ->
            incomingScore
                & Score.toAudio
                & Audio.toVector
                & show
                & T.pack
                & Cmd.log
            
            

playResponse :: Maybe Error -> Response
playResponse 
    = Response.json
    . Json.maybe
    . fmap (Json.string . Error.throw)


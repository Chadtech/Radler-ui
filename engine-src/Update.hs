{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

import Flow
import Prelude.Extra

import Audio (Audio)
import qualified Audio
import Cmd (Cmd)
import qualified Cmd
import qualified Data.List as List
import Data.Text.Lazy (Text)
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

        Play (Right score) ->
            ( Model.setScore score model
            , playScore score model
            , Response.json Json.null
            )

        Play (Left err) ->
            ( model
            , Cmd.none
            , Response.error
                400
                $ replaceChar 
                    '\n' 
                    ' ' 
                    (Error.throw err)
            )

        Build (Right score) ->
            ( model
            , buildScore score
            , Response.json Json.null
            )

        Build (Left err) ->
            ( model
            , Cmd.none
            , Response.error
                400
                $ Error.throw err
            )


buildScore :: Score -> Cmd
buildScore incomingScore =
    incomingScore
        |> Score.build
        |> indexList
        |> List.map (buildPart incomingScore)
        |> Cmd.batch


buildPart :: Score -> (Int, Audio) -> Cmd
buildPart score (index, audio) =
    Audio.write
        (Score.buildFilename score index)
        audio


playScore :: Score -> Model -> Cmd
playScore incomingScore model =
    let
        filename :: Text
        filename =
            Score.devFilename incomingScore
    in
    case Model.score model of
        Just existingScore ->
            case Score.diff incomingScore existingScore of
                Nothing ->
                    Audio.play filename

                Just _ ->
                    -- TO DO
                    writeAndPlay filename incomingScore
            
        Nothing ->
            writeAndPlay filename incomingScore
            

writeAndPlay :: Text -> Score -> Cmd
writeAndPlay filename score =
    [ score
        |> Score.toDevAudio
        |> Audio.write filename
    , Audio.play filename
    ]
        |> Cmd.batch

            
playResponse :: Maybe Error -> Response
playResponse = 
    Response.json
        <. Json.maybe
        <. fmap (Json.string . Error.throw)


{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

import Flow
import Prelude.Extra

import Audio (Audio)
import qualified Audio
import Cmd (Cmd)
import qualified Cmd
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Debug.Trace
import Error (Error)
import qualified Error
import Json (Json)
import qualified Json
import Msg (Msg(..))
import Model (Model)
import qualified Model
import qualified Part
import Part (Part)
import Response (Response)
import qualified Response
import Resolution (Resolution)
import qualified Resolution
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
            playScore score model

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


playScore :: Score -> Model -> (Model, Cmd, Response)
playScore incomingScore model =
    let
        filename :: Text
        filename =
            Score.devFilename incomingScore


        writeAndPlayCmd :: Audio -> Cmd
        writeAndPlayCmd audio =
            [ Audio.write filename audio
            , Audio.play filename
            ]
                |> Cmd.batch            


        writeAndPlayFromScratch :: (Model, Cmd, Response)
        writeAndPlayFromScratch =
            let
                audio :: Audio
                audio =
                    Score.toDevAudio incomingScore
            in
            ( Model.HasScore incomingScore audio
            , writeAndPlayCmd audio
            , Response.json Json.null
            )            
    in
    case model of
        Model.HasScore existingScore existingAudio ->
            let 
                diff :: Either Error (Resolution (List Part))
                diff =
                    Score.diff
                        incomingScore
                        existingScore
                        |> Either.mapLeft Error.ScoreError
                        |> debugLog "Resolution" show
            in
            case diff of 
                Right (Score.Changes (toRemove, toAdd)) ->
                    let
                        newAudio :: Audio
                        newAudio =
                            existingAudio
                                |> Audio.subtract (Part.manyToDevAudio toRemove)
                                |> Audio.mix (Part.manyToDevAudio toAdd)
                    in            
                    ( Model.HasScore incomingScore newAudio 
                    , writeAndPlayCmd newAudio 
                    , Response.json Json.null
                    )

                Right Score.Identical ->
                    ( model
                    , Audio.play filename
                    , Response.json Json.null
                    )

                Right Score.Unresolvable ->
                    writeAndPlayFromScratch
                    
                Left error ->
                    ( model
                    , Cmd.none
                    , Response.error
                        500
                        (Error.throw error)
                    )

        Model.Init ->
            writeAndPlayFromScratch




         

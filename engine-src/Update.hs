{-# LANGUAGE OverloadedStrings #-}


module Update (update) where

import Flow
import Prelude.Extra

import Audio (Audio)
import qualified Audio
import qualified Control.Monad as CM
import qualified Data.Either.Extra as Either
import qualified Data.List as List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Error (Error)
import qualified Error
import Index (Index)
import qualified Index
import Json (Json)
import qualified Json
import Model (Model)
import qualified Model
import qualified Part
import Part (Part)
import qualified Program
import Response (Response)
import qualified Response
import Resolution (Resolution)
import qualified Resolution
import Route (Route(..))
import qualified Route
import Score (Score)
import qualified Score
import qualified Web.Scotty.Trans as Web


update :: Maybe Route -> Model -> Response
update maybeRoute model =
    case maybeRoute of
        Nothing ->
            Response.error
                404
                "end point unsupported"

        Just route ->
            handleRoute route model


handleRoute :: Route -> Model -> Response
handleRoute route model =
    case route of
        Ping ->
            Response.ping 
            
        Echo body ->
            Response.text body 

        Play (Right score) ->
            playScore score model

        Play (Left err) ->
            err
                |> Error.throw
                |> replaceChar '\n' ' '
                |> Response.error 400

        Build (Right score) ->
            Web.liftAndCatchIO 
                (buildScore score)
                >> Response.json Json.null

        Build (Left err) ->
            err
                |> Error.throw
                |> Response.error 400


buildScore :: Score -> IO ()
buildScore incomingScore =
    let
        buildPart :: (Index, Audio) -> IO ()
        buildPart (index, audio) =
            Audio.write
                (Score.buildFilename incomingScore index)
                audio
    in
    incomingScore
        |> Score.build
        |> Index.list
        |> List.map buildPart
        |> CM.sequence_


playScore :: Score -> Model -> Response
playScore incomingScore model =
    let
        filename :: Text
        filename =
            Score.devFilename incomingScore


        writeAndPlayCmd :: Audio -> IO ()
        writeAndPlayCmd audio =
            [ Audio.write filename audio
            , Audio.play filename
            ]
                |> CM.sequence_          


        writeAndPlayFromScratch :: Response
        writeAndPlayFromScratch =
            let
                audio :: Audio
                audio =
                    Score.toDevAudio incomingScore
            in
            Program.setModel 
                (Model.HasScore incomingScore audio)
                >> Web.liftAndCatchIO 
                    (writeAndPlayCmd audio)
                >> Response.json Json.null
                
    in
    case model of
        Model.HasScore existingScore existingAudio ->
            case
                Score.diff
                    (Score.Incoming incomingScore)
                    (Score.Existing existingScore)
                    |> Either.mapLeft Error.ScoreError
            of
                Right (Score.Changes (toRemove, toAdd)) ->
                    let
                        newAudio :: Audio
                        newAudio =
                            existingAudio
                                |> Audio.subtract (Part.manyToDevAudio toRemove)
                                |> Audio.mix (Part.manyToDevAudio toAdd)
                    in            
                    Program.setModel
                        (Model.HasScore incomingScore newAudio)
                        >> Web.liftAndCatchIO 
                            (writeAndPlayCmd newAudio)
                        >> Response.json Json.null

                Right Score.Identical ->
                    Web.liftAndCatchIO 
                        (Audio.play filename)
                        >> Response.json Json.null

                Right Score.Unresolvable ->
                    writeAndPlayFromScratch
                    
                Left error ->
                    error
                        |> Error.throw
                        |> Response.error 500

        Model.Init ->
            writeAndPlayFromScratch




         

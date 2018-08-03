module Model 
    ( Model
    , name
    , project
    , fromProjectData
    )
    where


import qualified Data.Part as Part
import qualified Data.Project as Project
import Data.ByteString (ByteString)
import qualified Data.ByteString as Byte
import Result (Result)
import qualified Result
import qualified Data.List as List
import qualified Data.ByteString.Char8 as Char
import Flow
import Error (Error(ProjectError))


data Model
    = Model
        { project :: Project.Model
        }


name :: Model -> String
name model =
    Project.name (project model)


fromProjectData :: ByteString -> Result Error Model
fromProjectData byteString =
    getProjectString byteString
        |> Project.fromString
        |> Result.mapError ProjectError
        |> Result.map fromProject


fromProject :: Project.Model -> Model
fromProject project =
    Model
        { project = project }


getProjectString :: ByteString -> String
getProjectString projectData = 
    projectData
        |> Char.split '\n'
        |> List.map Char.unpack
        |> List.unlines
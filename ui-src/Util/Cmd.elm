module Util.Cmd exposing
    ( mapCmd
    , mapModel
    , withModel
    , withNoCmd
    )


mapCmd : (a -> b) -> ( model, Cmd a ) -> ( model, Cmd b )
mapCmd f ( model, cmd ) =
    ( model, Cmd.map f cmd )


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )


withModel : model -> Cmd msg -> ( model, Cmd msg )
withModel =
    Tuple.pair


mapModel : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
mapModel =
    Tuple.mapFirst

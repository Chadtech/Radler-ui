module Colors
    exposing
        ( background0
        , background1
        , background2
        , background3
        , background4
        , critical
        , good
        , highlight0
        , highlight1
        , ignorable0
        , ignorable1
        , ignorable2
        , ignorable3
        , important0
        , important1
        , lowWarning
        , point0
        , point1
        , prettyBlue
        )

{-| Colors in the Chadtech design standard v1.1


# Background

@docs background2, background1, background3, background0


# Point

@docs point0, point1, important0, important1


# Ignorables

@docs ignorable0, ignorable1, ignorable2, ignorable3


# High Saturation Colors

@docs good, critical, prettyBlue, lowWarning

-}

import Css exposing (Color, hex)


highlight0 : Color
highlight0 =
    hex "#142909"


highlight1 : Color
highlight1 =
    hex "#30371a"


background4 : Color
background4 =
    hex "#082221"


{-| A lighter version of `background2`. Usually used to highlight list items against `background2`

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #071d17"></div>

-}
background3 : Color
background3 =
    hex "#071d17"


{-| `background2` is the default background color. Not totally black, and slightly turqoise, but still dark

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #06120e"></div>

-}
background2 : Color
background2 =
    hex "#06120e"


{-| A darker version of background2. Could be mistaken as black. This color exists mainly to emphasize the visual effect of being in the background, or for contrast against `background2`

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #030907"></div>

-}
background1 : Color
background1 =
    hex "#030907"


{-| Actual black. Should rarely be used, but used to demonstrate absolute minimum brightness.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #000000"></div>

-}
background0 : Color
background0 =
    hex "#000000"


{-| This color is for things that demand attention and must be noticed. It should be used sparingly, if at all.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #f21d23"></div>

-}
critical : Color
critical =
    hex "#f21d23"


{-| Warnings, but not the "critical melt down" sort, more the "something isnt right" sort.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #651a20"></div>

-}
lowWarning : Color
lowWarning =
    hex "#651a20"


{-| When things are successful, this color is used.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #366317"></div>

-}
good : Color
good =
    hex "#366317"


{-| When things need to be decorated, they are blue. Blue is the color to make things look pretty. Its the color for things that are special.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #175cfe"></div>

-}
prettyBlue : Color
prettyBlue =
    hex "#175cfe"


{-| When some text needs even more priority or attention

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #e3d34b"></div>

-}
important1 : Color
important1 =
    hex "#e3d34b"


{-| When some text needs priority or attention

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #b39f4b"></div>

-}
important0 : Color
important0 =
    hex "#b39f4b"


{-| `point0`, but brighter. Useful for lighting up `poi0 during a hover over.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #e0d6ca"></div>

-}
point1 : Color
point1 =
    hex "#e0d6ca"


{-| Point, the default color of text and content. If things are the content that should be presented, then they should be colored `point0`. They are "the point" of the web app.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #b0a69a"></div>

-}
point0 : Color
point0 =
    hex "#b0a69a"


{-| This color is called "ignorable", its for unimportant things the user can be forgotten.

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #807672"></div>

-}
ignorable0 : Color
ignorable0 =
    hex "#807672"


{-| `ignorable1` is darker than `ignorable0`

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #57524f"></div>

-}
ignorable1 : Color
ignorable1 =
    hex "#57524f"


{-| `ignorable2` is darker than `ignorable1`

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #2c2826"></div>

-}
ignorable2 : Color
ignorable2 =
    hex "#2c2826"


{-| `ignorable3` is darker than `ignorable2`

<div style="border: 2px solid #000000; width: 200px; height: 200px; background-color: #131610"></div>

-}
ignorable3 : Color
ignorable3 =
    hex "#131610"

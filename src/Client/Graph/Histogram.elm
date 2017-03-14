module Client.Graph.Histogram exposing (..)

import Dict
import Graph exposing (Graph)
import Graph.Node exposing (Node)
import Plot exposing (plot)
import Plot.Line as Line
import Plot.Area as Area
import Plot.Axis as Axis
import Svg exposing (..)
import Graph.Degree as Degree exposing (Degree)


view : Graph Node -> Svg a
view graph =
    let
        degrees =
            Degree.topDegrees graph

        imports =
            degreesToHistogram degrees.outgoing .outgoing
                |> List.map (Tuple.mapSecond ((*) -1))

        importees =
            degreesToHistogram degrees.incoming .incoming

        degreesToHistogram d selector =
            List.map (\( ident, degree ) -> selector degree) d
                |> histogram
                |> listToFloat

        listToFloat list =
            list
                |> List.map ((Tuple.mapFirst toFloat) >> (Tuple.mapSecond toFloat))
    in
        plot
            [ Plot.size plotSize
            , Plot.margin ( 10, 20, 40, 20 )
            ]
            [ Plot.area
                [ Area.stroke "#f00"
                , Area.fill "#ff0"
                , Area.strokeWidth 2
                ]
                imports
            , Plot.area
                [ Area.stroke "#00f"
                , Area.fill "#0ff"
                , Area.strokeWidth 2
                ]
                importees
            , Plot.xAxis
                [ Axis.line [ Line.stroke "#000" ]
                ]
            ]


plotSize : ( Int, Int )
plotSize =
    ( 600, 250 )


histogram : List comparable -> List ( Int, comparable )
histogram values =
    List.foldl
        (\v dict ->
            Dict.insert v
                ((Dict.get v dict |> Maybe.withDefault 0) + 1)
                dict
        )
        Dict.empty
        values
        |> Dict.toList

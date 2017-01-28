module Inspection exposing (..)

import Analyser.FileContext as FileContext
import Analyser.LoadedDependencies exposing (LoadedDependencies)
import Analyser.Messages exposing (Message)
import Analyser.Types exposing (LoadedSourceFiles)
import Analyser.Checks.UnusedVariable as UnusedVariable


run : LoadedSourceFiles -> LoadedDependencies -> List Message
run source deps =
    let
        messages =
            source
                |> List.filterMap (FileContext.create source deps)
                -- |> List.drop 3
                -- |> List.head
                |>
                    List.concatMap UnusedVariable.scan
    in
        messages

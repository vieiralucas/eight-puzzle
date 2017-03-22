module Search exposing (aStar)

import Board exposing (Board)
import Matrix


aStar : Board -> List Matrix.Location
aStar board =
    -- TODO
    [ Matrix.loc 2 1, Matrix.loc 1 1, Matrix.loc 1 2 ]

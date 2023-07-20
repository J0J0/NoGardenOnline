# NoGardenOnline
Port of [NoGardenPuzzle](https://github.com/J0J0/games-poc/tree/main/NoGardenPuzzle)
to the [reflex(-dom)](https://hackage.haskell.org/package/reflex-dom)
[FRP](https://wiki.haskell.org/Functional_Reactive_Programming) framework
running off [obelisk](https://github.com/obsidiansystems/obelisk).

## Build instructions
Follow the [obelisk install instructions](https://github.com/obsidiansystems/obelisk/#installing-obelisk).

Then

    ob run

takes care of the rest and starts a server at <http://localhost:8000>.

## Game rules
The goal is to fill the rectangular game field with horizontal and vertical lines,
such that each line starts and ends "outside" of the field. A started line extends
in a straight manner until it hits a blocked tile or the outside. In the former
case, the line must be continued in either possible direction.

module Day11 exposing(..)

import Html exposing(Html, text, div, span)
import Html.Attributes exposing (style)
{-
--- Day 11: Radioisotope Thermoelectric Generators ---

You come upon a column of four floors that have been entirely sealed off from the rest of the building except for a small dedicated lobby. There are some radiation warnings and a big sign which reads "Radioisotope Testing Facility".

According to the project status board, this facility is currently being used to experiment with Radioisotope Thermoelectric Generators (RTGs, or simply "generators") that are designed to be paired with specially-constructed microchips. Basically, an RTG is a highly radioactive rock that generates electricity through heat.

The experimental RTGs have poor radiation containment, so they're dangerously radioactive. The chips are prototypes and don't have normal radiation shielding, but they do have the ability to generate an electromagnetic radiation shield when powered. Unfortunately, they can only be powered by their corresponding RTG. An RTG powering a microchip is still dangerous to other microchips.

In other words, if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the chip will be fried. Therefore, it is assumed that you will follow procedure and keep chips connected to their corresponding RTG when they're in the same room, and away from other RTGs otherwise.

These microchips sound very interesting and useful to your current activities, and you'd like to try to retrieve them. The fourth floor of the facility has an assembling machine which can make a self-contained, shielded computer for you to take with you - that is, if you can bring it all of the RTGs and microchips.

Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is an elevator that can move between the four floors. Its capacity rating means it can carry at most yourself and two RTGs or microchips in any combination. (They're rigged to some heavy diagnostic equipment - the assembling machine will detach it for you.) As a security measure, the elevator will only function if it contains at least one RTG or microchip. The elevator always stops on each floor to recharge, and this takes long enough that the items within it and the items on that floor can irradiate each other. (You can prevent this if a Microchip and its Generator end up on the same floor in this way, as they can be connected while the elevator is recharging.)

You make some notes of the locations of each component of interest (your puzzle input). Before you don a hazmat suit and start moving things around, you'd like to have an idea of what you need to do.

When you enter the containment area, you and the elevator will start on the first floor.

For example, suppose the isolated area has the following arrangement:

The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.
As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for Generator), the initial state looks like this:

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 .  HG .  .  .  
F1 E  .  HM .  LM 
Then, to get everything up to the assembling machine on the fourth floor, the following steps could be taken:

Bring the Hydrogen-compatible Microchip to the second floor, which is safe because it can get power from the Hydrogen Generator:

F4 .  .  .  .  .  
F3 .  .  .  LG .  
F2 E  HG HM .  .  
F1 .  .  .  .  LM 
Bring both Hydrogen-related items to the third floor, which is safe because the Hydrogen-compatible microchip is getting power from its generator:

F4 .  .  .  .  .  
F3 E  HG HM LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  LM 
Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible Microchip back down with you so you can still use the elevator:

F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 E  .  HM .  .  
F1 .  .  .  .  LM 
At the first floor, grab the Lithium-compatible Microchip, which is safe because Microchips don't affect each other:

F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 .  .  .  .  .  
F1 E  .  HM .  LM 
Bring both Microchips up one floor, where there is nothing to fry them:

F4 .  .  .  .  .  
F3 .  HG .  LG .  
F2 E  .  HM .  LM 
F1 .  .  .  .  .  
Bring both Microchips up again to floor three, where they can be temporarily connected to their corresponding generators while the elevator recharges, preventing either of them from being fried:

F4 .  .  .  .  .  
F3 E  HG HM LG LM 
F2 .  .  .  .  .  
F1 .  .  .  .  .  
Bring both Microchips to the fourth floor:

F4 E  .  HM .  LM 
F3 .  HG .  LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  
Leave the Lithium-compatible microchip on the fourth floor, but bring the Hydrogen-compatible one so you can still use the elevator; this is safe because although the Lithium Generator is on the destination floor, you can connect Hydrogen-compatible microchip to the Hydrogen Generator there:

F4 .  .  .  .  LM 
F3 E  HG HM LG .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  
Bring both Generators up to the fourth floor, which is safe because you can connect the Lithium-compatible Microchip to the Lithium Generator upon arrival:

F4 E  HG .  LG LM 
F3 .  .  HM .  .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  
Bring the Lithium Microchip with you to the third floor so you can use the elevator:

F4 .  HG .  LG .  
F3 E  .  HM .  LM 
F2 .  .  .  .  .  
F1 .  .  .  .  .  
Bring both Microchips to the fourth floor:

F4 E  HG HM LG LM 
F3 .  .  .  .  .  
F2 .  .  .  .  .  
F1 .  .  .  .  .  
In this arrangement, it takes 11 steps to collect all of the objects at the fourth floor for assembly. (Each elevator stop counts as one step, even if nothing is added to or removed from it.)

In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?

-}

partOneOutput = div [ style [ ("font-family", "monospace") ] ] (renderFacility facility)

type Object = Microchip Char
            | Generator Char
            | Nothing

type Status = Valid
            | Invalid
            | Solved


main = partOneOutput

type alias Facility = { elevator: Int
                      , items: List (Int, Object)
                      }

facility = Facility 1 [ (1, Generator 'T')
                      , (1, Microchip 'T')
                      , (1, Generator 'P')
                      , (1, Generator 'S')
                      , (2, Microchip 'P')
                      , (2, Microchip 'S')
                      , (3, Generator 'Q')
                      , (3, Microchip 'Q')
                      , (3, Generator 'R')
                      , (3, Microchip 'R')
                      ]

facility_invalid = Facility 1 [ (1, Generator 'T')
                      , (1, Microchip 'U')
                      , (1, Generator 'P')
                      , (1, Generator 'S')
                      , (2, Microchip 'P')
                      , (2, Microchip 'S')
                      , (3, Generator 'Q')
                      , (3, Microchip 'Q')
                      , (3, Generator 'R')
                      , (3, Microchip 'R')
                      ]

facility_solved = Facility 1 [ (4, Generator 'T')
                      , (4, Microchip 'T')
                      , (4, Generator 'P')
                      , (4, Generator 'S')
                      ]

facilityState : Facility -> Status
facilityState facility =
    let
        getFloor items i = List.filter (\(index, value) -> index == i) items |> List.map (\(i, v) -> v)
        floorStates = List.map floorValid <| List.map (getFloor facility.items) [1, 2, 3, 4]
        valid = List.foldl (\a acc -> acc && a) True floorStates
        solved = List.filter (\(index, value) -> not (index == 4)) facility.items |> List.length |> (==) 0
    in
        if solved then
            Solved
        else if valid then
            Valid
        else
            Invalid


floorValid : List Object -> Bool
floorValid objects =
    case objects of
        [] -> True
        _ ->
            let
                generators = List.filter generatorFilter objects
                microchips = List.filter microchipFilter objects
            in
                List.foldl (\c acc -> acc && (List.length generators == 0 || List.any (objMatch c) generators)) True microchips

movesFromFloor : List Object -> List (Object, Object)
movesFromFloor floor =
    case floor of
        x::xs ->
            let
                bs = List.repeat (List.length xs) x
                solo = [ (x, Nothing) ]
            in
                List.map2 (,) bs xs ++ solo ++ movesFromFloor xs
        [] -> []

objMatch : Object -> Object -> Bool
objMatch match against =
    case match of
        Microchip a ->
            case against of
                Generator b -> a == b
                _ -> False
        Generator c ->
            case against of
                Microchip d -> d == c
                _ -> False
        Nothing -> False

microchipFilter : Object -> Bool
microchipFilter obj =
                    case obj of
                        Microchip _ -> True
                        _ -> False

generatorFilter : Object -> Bool
generatorFilter obj =
                    case obj of
                        Generator _ -> True
                        _ -> False

renderFacility : Facility -> List (Html msg)
renderFacility facility = List.map (renderFloor facility) [1, 2, 3, 4]

spaced = [ style [ ("padding", "5px") ] ]

renderFloor : Facility -> Int -> Html msg
renderFloor facility index =
    let
        items = List.filter (\(i, v) -> i == index) facility.items |> List.map (\(i, v) -> renderObject v)
    in
        div [] ([ span [] [ text ("F" ++ (toString index)) ]
                            , span spaced [ text (if facility.elevator == index then "E" else ".") ]
                            ] ++ items)

show char id = String.reverse <| String.cons char id

renderObject : Object -> Html msg
renderObject object =
    let
        chip c = show c "M"
        gen g = show g "G"
    in
        case object of
            Microchip m -> span spaced [ text (chip m) ]
            Generator g -> span spaced [ text (gen g) ]
            Nothing -> span spaced [ text "." ]

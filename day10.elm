module Day10 exposing(..)
import String exposing (words)
import List exposing(head)

type Chip = Value Int | None
type Give = Destination Int
type alias Bot = { giveHigh: Give
                 , giveLow: Give
                 , chips: (Chip, Chip)
                 }

-- eg bot id gives high to x low to y
type alias DefineBot = { high: Int
                       , low: Int
                       , id: Int
                       }

-- eg value x goes to bot y
type alias SetBotValue = { id: Int
                         , value: Int
                         }

type BotInstruction = Define DefineBot | Set SetBotValue

parseInstruction : String -> BotInstruction
parseInstruction input =
    let
        parts = words input
    in
        case head parts of
            Just "bot" -> parseBotDefinition parts
            Just "value" -> Set <| SetBotValue 1 1
            _ -> Debug.crash ("bizarre instruction: " ++ input)

parseBotDefinition : List String -> BotInstruction
parseBotDefinition rawBot =
    case rawBot of
        _::botId::_::_::_::_::low::_::_::_::_::high::_ ->
            case parseThreeInts high low botId of
                (Ok a, Ok b, Ok c) ->
                    Define <| DefineBot a b c
                _ -> Debug.crash "bad integers in bot defintion"
        _ -> Debug.crash "tried to parse a weird bot definition"

parseBotValue : List String -> BotInstruction
parseBotValue rawValue =
    case rawValue of
        _::value::_::_::_::botId::_ ->
            case (String.toInt value, String.toInt botId) of
                (Ok v, Ok id) ->
                    Set <| SetBotValue id v
                _ -> Debug.crash "bad bot value"
        _ -> Debug.crash "bad bot value"

parseThreeInts a b c =
    (String.toInt a, String.toInt b, String.toInt c)

// Learn more about F# at http://fsharp.org

open System
open Thoth.Json.Net
open SpacedRepetition.Net
open SpacedRepetition.Net.ReviewStrategies

type vocab = { French : string; English : string }
type vocabReviewItem = ReviewItem<vocab>

let getNow() = DateTime.Now

let help() =
    printfn "Hit enter to get translation. Outcome: 1 Perfect 2 Hesitant 3 Incorrect. ? to display this"

let quiz (session : StudySession<vocab>) (item : vocabReviewItem) : vocabReviewItem =
    let rec getInput() : ReviewOutcome Option =
        let userOutcome = Console.ReadKey().KeyChar
        printfn ""
        match userOutcome with 
        | '1' -> Some ReviewOutcome.Perfect
        | '2' -> Some ReviewOutcome.Hesitant
        | '3' -> Some ReviewOutcome.Incorrect
        | '?' -> help()
                 getInput()
        | _ ->   None

    let rec needInput() : ReviewOutcome =
        match getInput() with
        | Some outcome -> outcome
        | None -> needInput()

    printfn "français: %s" item.Item.French

    let outcomeEarly = getInput()

    printfn "anglais: %s" item.Item.English
    printfn ""
    
    let result = 
        match outcomeEarly with
        | Some outcome -> outcome
        | None -> 
            needInput()
    
    session.Review(item, result)

[<EntryPoint>]
let main argv =
    let vocabSet : vocabReviewItem List  = [
        {
            Status = NeverReviewed
            Item = { French = "jamais"; English = "never" }
        }
        {
            Status = NeverReviewed
            Item = { French = "peut-être"; English = "maybe" } 
        }
        ]
    let strategy = new SuperMemo2ReviewStrategy(getNow)
    let studySession = new StudySession<vocab>(vocabSet, getNow, strategy, 3, 3)
    let mapWithQuiz = quiz studySession
    help()
    let result = List.map mapWithQuiz vocabSet
    let json = Encode.Auto.toString(4, result)
    printfn "json: %s" json
    0 // return an ok exit code

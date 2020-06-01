(*

Algorithm SM-2, (C) Copyright SuperMemo World, 1991.

https://www.supermemo.com
https://www.supermemo.eu

*)
open System
open System.IO
open Thoth.Json.Net
open SpacedRepetition.Net
open SpacedRepetition.Net.ReviewStrategies

type vocab = { French : string; English : string }

type UserAction = ReviewOutcome of ReviewOutcome | Quit

let getNow() = DateTime.Now

let help() =
    printfn "Hit enter to get translation. Outcome: 0 Stop 1 Perfect 2 Hesitant 3 Incorrect. ? to display this"

let quizzer (session : StudySession<vocab>) (quizResults : ReviewItem<vocab> List) : 
    (ReviewItem<vocab> * ReviewItem<vocab> List) option =
    let rec getInput() : UserAction Option =
        let userOutcome = Console.ReadKey().KeyChar
        printfn ""
        match userOutcome with
        | '0' -> Some Quit
        | '1' -> Some (ReviewOutcome ReviewOutcome.Perfect)
        | '2' -> Some (ReviewOutcome ReviewOutcome.Hesitant)
        | '3' -> Some (ReviewOutcome ReviewOutcome.Incorrect)
        | '?' -> help()
                 getInput()
        | _ ->   None

    let rec needInput() : UserAction =
        match getInput() with
        | Some outcome -> outcome
        | None -> needInput()

    match Seq.tryHead session with
    | None -> None
    | Some item ->    
        Console.Clear();
        printfn "français: %s" item.Item.French

        let outcomeEarly = getInput()

        printfn "anglais: %s" item.Item.English
        printfn ""
    
        let action = 
            match outcomeEarly with
            | Some outcome -> outcome
            | None -> needInput()
    
        match action with
        | Quit -> None
        | ReviewOutcome result -> 
            let reviewedItem = session.Review(item, result)
            Some (reviewedItem, reviewedItem::quizResults)

let rec AskVocabList() =
    printfn "Enter filename of json or enter for default: "
    let line = Console.ReadLine().Trim()
    let line = if line = "" then "vocab.txt" else line
    if not (File.Exists(line)) then
        printfn "error finding file. restarting..."
        AskVocabList()
    else
        let text = File.ReadAllText(line)
        match (Decode.Auto.fromString text) with
        | Ok data ->
            printfn "loaded %s" line
            data
        | _ -> 
            printfn "error loading data. restarting..."
            AskVocabList()

let merge (primary : ReviewItem<vocab> List) (secondary : ReviewItem<vocab> List) =
    let extractVocab (i : ReviewItem<vocab>) = i.Item
    let addIfNotPresent state (item : ReviewItem<vocab>) =
        if (List.map extractVocab state) |> List.contains item.Item then
            state
        else
            item::state
    List.fold addIfNotPresent [] (primary@secondary)

[<EntryPoint>]
let main argv =
    let vocabSet : ReviewItem<vocab> List  = AskVocabList()
    let strategy = new SuperMemo2ReviewStrategy(getNow)
    let studySession = new StudySession<vocab>(vocabSet, getNow, strategy, 3, 3)
    let stepQuiz = quizzer  studySession
    help()
    printfn "Press enter to start..."
    Console.ReadKey() |> ignore;
    let sessionResult = Seq.toList (Seq.unfold stepQuiz [])
    // merge result with original list, remove originals if a match...
    let result : ReviewItem<vocab> List = (merge sessionResult vocabSet)
    let json = Encode.Auto.toString(4, result)
    printfn "Enter to save to disk or ctrl-c exit..."
    Console.ReadLine() |> ignore
    File.WriteAllText("vocab.txt", json)
    0 // return an ok exit code

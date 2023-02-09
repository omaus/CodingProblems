(* 
Task:
Take an input list and return the list until there are two consecutive zeroes.
E.g.:
input list could be: [10; 9; 8; 7; 7; 6; 6; 5; 5; 4; 4; 4; 3; 3; 2; 2; 1; 0; 0; 0; -1; -2; -3]
output list should be: [[10; 9; 8; 7; 7; 6; 6; 5; 5; 4; 4; 4; 3; 3; 2; 2; 1; 0; 0]
Constraint: Do this in a functional way, without using loops.
*)

let getTill00 inputList =
    let rec loop il finalList lastNo =
        match il with
        | h :: t -> 
            if h = 0 && lastNo = 0 then 
                h :: finalList
            else
                loop t (h :: finalList) h
        | _ -> failwith "InputList has no 2 consecutive 0s."
    loop inputList [] inputList.Head |> List.rev

getTill00 [10; 9; 8; 7; 7; 6; 6; 5; 5; 4; 4; 4; 3; 3; 2; 2; 1; 0; 0; 0; -1; -2; -3]
getTill00 [10; 9; 8; 7; 7; 6; 6; 5; 5; 4; 4; 4; 3; 3; 2; 2; 1; 0]
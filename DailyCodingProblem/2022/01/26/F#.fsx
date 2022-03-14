let rand7 () = System.Random().Next(1, 7)

let rec rand5 () = 
    let n = rand7 ()
    if n > 5 then rand5 ()
    else n

rand5 ()
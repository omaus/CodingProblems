// Task:
// Format an RGB value (three 1-byte numbers) as a 6-digit hexadecimal string.
// taken from: https://sites.google.com/site/steveyegge2/five-essential-phone-screen-questions

type RGB = {
    R : byte
    G : byte
    B : byte
}

let rgbToHex rgb =
    let numToHex num =
        match num with
        | x when x < 10 -> string x
        | _ -> num + 55 |> char |> string
    let byteToHex by =
        let f = by / 16
        let s = by % 16
        $"{numToHex f}{numToHex s}"
    $"#{int rgb.R |> byteToHex}{int rgb.G |> byteToHex}{int rgb.B |> byteToHex}"

rgbToHex {R = 0uy; G = 45uy; B = 255uy}
// Task: 
// Print all times tables (from 1 to 12) for all numbers from 1 to 12.
// taken from: https://qr.ae/pG08Sm

for i = 1 to 12 do 
    printfn $"Table for {i} (from {i} to {i * 12}):"
    for j = 1 to 12 do printfn $"{i * j}"
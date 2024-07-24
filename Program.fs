let rec printSalaries salaryList =
    match salaryList with
    | [] -> ()
    | head :: tail ->
        printf "%d " head
        printSalaries tail

let salaryList = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

printf "All Salaries: "
printSalaries salaryList



// Filter high-income salaries
let highIncomeSalaries = List.filter (fun salary -> salary > 100000) salaryList

printf "\nHigh-Income Salaries: "
printSalaries highIncomeSalaries



// Define a function to calculate tax based on the salary
let solvingTax salary =
    match salary with
    | s when s <= 49020 -> float s * 0.15
    | s when s <= 98040 -> float s * 0.205
    | s when s <= 151978 -> float s * 0.26
    | s when s <= 216511 -> float s * 0.29
    | s -> float s * 0.33




// Calculate taxes for all salaries
let taxList = List.map solvingTax salaryList



// Define a function to print a list of taxes
let rec printTaxes taxList =
    match taxList with
    | [] -> ()
    | head :: tail ->
        printf "%.2f " head
        printTaxes tail

printf "\nTaxes for Salaries: "
printTaxes taxList



// Increase salaries less than $49,020 by $20,000
let NewSalaries1 =
    salaryList
    |> List.filter (fun salary -> salary < 49020)
    |> List.map (fun salary -> salary + 20000)

printf "\nSalaries (less than $49,020 increased by $20,000): "
printSalaries NewSalaries1



// Sum salaries between $50,000 and $100,000
let totalInRange =
    salaryList
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0

printf "\nSum of Salaries between $50,000 and $100,000: %d" totalInRange



//Tail Recursion


let Sum3 (n: int) =
    let rec helper (current: int) (acc: int) =
        if current <= 0 then acc
        else helper (current - 3) (acc + current)
    helper n 0

let result = Sum3 45

printfn "The sum of all multiples of 3 up to 45 is: %d" result

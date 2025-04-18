open System


let generatePellSolutions n =    
    let x = [9I]
    let y = [2I]
    let rec generateNext i xList yList =
        if i < n then
            let k1 = (List.head xList) * (xList.[i-1]) + 20I * (List.head yList) * (yList.[i-1])
            let k2 = (List.head xList) * (yList.[i-1]) + (List.head yList) * (xList.[i-1])
            generateNext (i + 1) (xList@[k1]) (yList@[k2]) 
        else
            (xList, yList)    
    generateNext 1 x y 


let generateExtendedSolutions n (p1: bigint list) (q1: bigint list) (p2: bigint list) (q2: bigint list) (p3: bigint list) (q3: bigint list) (x: bigint list) (y: bigint list) =
    let rec loop i (p1, q1, p2, q2, p3, q3) =
        if i < n then
            
            let k1 = List.head p1 * x.[i] + 20I * List.head q1 * y.[i]
            let k2 = List.head p1 * y.[i] + List.head q1 * x.[i]
            let k3 = List.head p1 * x.[i] - 20I * List.head q1 * y.[i]
            let k4 = List.head p1 * y.[i] - List.head q1 * x.[i]
            let p1' = p1 @ [k1; k3]
            let q1' = q1 @ [k2; k4]

            let k1 = List.head p2 * x.[i] + 20I * List.head q2 * y.[i]
            let k2 = List.head p2 * y.[i] + List.head q2 * x.[i]
            let k3 = List.head p2 * x.[i] - 20I * List.head q2 * y.[i]
            let k4 = List.head p2 * y.[i] - List.head q2 * x.[i]
            let p2' = p2 @ [k1; k3]
            let q2' = q2 @ [k2; k4]

            let k1 = List.head p3 * x.[i] + 20I * List.head q3 * y.[i]
            let k2 = List.head p3 * y.[i] + List.head q3 * x.[i]
            let k3 = List.head p3 * x.[i] - 20I * List.head q3 * y.[i]
            let k4 = List.head p3 * y.[i] - List.head q3 * x.[i]
            let p3' = p3 @ [k1; k3]
            let q3' = q3 @ [k2; k4]

            loop (i + 1) (p1', q1', p2', q2', p3', q3')
        else
            [p1; q1; p2; q2; p3; q3]
    
    let z = loop 0 (p1, q1, p2, q2, p3, q3)
   
    
    z.[0]@z.[2]@z.[4]



let calculateGoldenNuggets (T: bigint list) =
    let mutable sum = 0I
    let mutable count = 0
    let mutable ret = 0
    while ret < T.Length && count < 31 do
        if (T.[ret] - 14I) % 10I = 0I then
            count <- count + 1
            Console.WriteLine("KFC #{0}: {1}" ,count, ((T.[ret] - 14I) / 10I))
            sum <- sum + ((T.[ret] - 14I) / 10I)
        ret <- ret + 1
    sum


[<EntryPoint>]
let main argv =
   
    let x, y = generatePellSolutions 20   
 
    let p1 = [14I]
    let q1 = [1I]
    let p2 = [16I]
    let q2 = [2I]
    let p3 = [26I]
    let q3 = [5I]
    
    
    let sortedT = 
        let p1New = generateExtendedSolutions 20 p1 q1 p2 q2 p3 q3 x y
        List.sort (p1New)
    
   
    let sum = calculateGoldenNuggets sortedT 
    Console.WriteLine("Сумма 30 штук : {0}",sum)
    
    0 

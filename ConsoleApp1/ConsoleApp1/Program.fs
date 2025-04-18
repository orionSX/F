open System

type Maybe<'T> =
    | Just of 'T
    | Nothing

// Функтор
let map f m =
    match m with
    | Just x -> Just (f x)
    | Nothing -> Nothing

// Аппликативный функтор
let apply mf mx =
    match mf, mx with
    | Just f, Just x -> Just (f x)
    | _ -> Nothing

let pure x  = Just x

// Монада
let returnM x = Just x

let bind m f =
    match m with
    | Just x -> f x
    | Nothing -> Nothing

[<EntryPoint>]
let main argv = 
    let testValue = 5
    let testF = fun x -> x + 1
    let testG = fun x -> x * 2
    Console.WriteLine((testF>>testG)(5))
    // Функтор — законы
    let law1 = 
        map id (Just testValue) = Just testValue

    let law2 = 
        map (testF >> testG) (Just testValue) = (map testG (map testF (Just testValue)))

    // Аппликативный функтор — законы
    let law3 = 
        apply (pure id) (Just testValue) = Just testValue

    let law4 = 
        apply (pure testF) (pure testValue) = pure (testF testValue)

    let law5 = 
        let u = Just testF
        apply u (pure testValue) = apply (pure (fun f -> f testValue)) u

    // Монада — законы
    let law6 = 
        bind (returnM testValue) (fun x -> Just (testF x)) = (fun x -> Just (testF x)) testValue

    let law7 = 
        bind (Just testValue) returnM = Just testValue

    let law8 = 
        let m = Just testValue
        bind (bind m (fun x -> Just (testF x))) (fun x -> Just (testG x)) = 
            bind m (fun x -> bind (Just (testF x)) (fun y -> Just (testG y)))

    // Вывод результатов
    Console.WriteLine("Проверка законов:")
    Console.WriteLine("Функтор:")
    Console.WriteLine("1. Идентичность: {0}", law1)
    Console.WriteLine("2. Композиция: {0}", law2)
    
    Console.WriteLine("\nАппликативный функтор:")
    Console.WriteLine("3. Идентичность: {0}", law3)
    Console.WriteLine("4. Гомоморфизм: {0}", law4)
    Console.WriteLine("5. Обмен (интерчейндж): {0}", law5)
    
    Console.WriteLine("\nМонада:")
    Console.WriteLine("6. Левая идентичность: {0}", law6)
    Console.WriteLine("7. Правая идентичность: {0}", law7)
    Console.WriteLine("8. Ассоциативность: {0}", law8)

    0

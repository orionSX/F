open System

// Определение типа сообщений
type AgentMessage =
    | Hello of string
    | Minus of int * int
    | Date
    | Quit

// Создание агента
let agent = 
    MailboxProcessor.Start(fun inbox ->
        async {
            while true do
                let! message = inbox.Receive()
                match message with
                | Hello name -> 
                    Console.WriteLine("Привет, {0}!",name)
                | Minus (a, b) ->                     
                    Console.WriteLine("{0} - {1} = {2}",a,b,a-b)
                | Date -> 
                    Console.WriteLine("Сейчас: {0}",DateTime.Now)
                | Quit -> 
                    Console.WriteLine("Завершение работы агента.")
                    Environment.Exit(0)
        }
    )


[<EntryPoint>]
let main argv =
    Console.WriteLine("Примеры работы агента:")

    agent.Post(Hello "Sanya")
    agent.Post(Minus(52, 10))
    agent.Post(Date)
    


 
    Async.Sleep(2000) |> Async.RunSynchronously

    agent.Post(Quit)

  
    agent.Post(Date)

    Console.WriteLine("Программа завершена.")
    0

open System

type IPrint =
    abstract Print : unit -> unit


type RealGeometricFigure=
    |Rectangle of width:float * height:float
    |Square of side:float
    |Circle of radius:float

let calcArea figure =
    match figure with
    | Rectangle(width, height) ->  Console.WriteLine("{0}",(width * height))
    | Square(side) -> Console.WriteLine("{0}",(side * side))
    | Circle(radius) -> Console.WriteLine("{0}",(Math.PI * radius ** 2.0))
let figs = [
    Rectangle(5.0,6.0);Square(5.0);Circle(2.0)
]
[<AbstractClass>]
type GeometricFigure() =
    abstract Area : float

type Rectangle(width: float, height: float) =
    inherit GeometricFigure()
    member this.Width = width
    member this.Height = height
    override this.Area = this.Width * this.Height
    override this.ToString() =
        
        String.Format("{0}, {1}", "Прямоугольник", String.Format("ширина: {0}, высота: {1}, площадь: {2}", this.Width, this.Height, this.Area))
    interface IPrint with
        member this.Print() = Console.WriteLine("{0}, {1}", "Print", this.ToString())

type Square(side: float) =
    inherit Rectangle(side, side)
    override this.ToString() = 
        String.Format("{0}, {1}", "Квадрат", String.Format("сторона: {0}, площадь: {1}", this.Width, this.Area))
    interface IPrint with
        member this.Print() = Console.WriteLine("{0}, {1}", "Print", this.ToString())

type Circle(radius: float) =
    inherit GeometricFigure()
    member this.Radius = radius
    override this.Area = Math.PI * this.Radius ** 2.0
    override this.ToString() = 
        String.Format("{0}, {1}", "Круг", String.Format("радиус: {0}, площадь: {1}", this.Radius, this.Area))
    interface IPrint with
        member this.Print() = Console.WriteLine("{0}, {1}", "Print", this.ToString())

let printFigure (figure: IPrint) = figure.Print()

let rect = Rectangle(11.0, 9.0)
let square = Square(5.0)
let circle = Circle(3.0)

printFigure rect
printFigure square
printFigure circle

Console.WriteLine("{0}, {1}", "ToString", rect.ToString())
Console.WriteLine("{0}, {1}", "ToString", square.ToString())
Console.WriteLine("{0}, {1}", "ToString", circle.ToString())

[<EntryPoint>]
let main argv = 
    figs |> List.iter calcArea
    0

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open System.Drawing
open System.Windows.Forms

let convertDataRow (csvLine:string)=
    let cells = List.ofSeq(csvLine.Split(','))
    match cells with
    | title::number::_ -> 
       let parsedNumber = Int32.Parse(number)
       (title, parsedNumber)
    | _ -> failwith "Incorrect data format!"

let rec processLines(lines)=
    match lines with
    | [] -> []
    | currentLine::remaining ->
        let parsedLine = convertDataRow(currentLine)
        let parsedRest = processLines(remaining)
        parsedLine :: parsedRest

let rec calculateSum(rows) =
    match rows with
    | [] -> 0
    | (_,value)::tail -> 
       let remainingSum =  calculateSum(tail)
       value + remainingSum




[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let lines = List.ofSeq(File.ReadAllLines(@"/Users/christian/GitHub/LearningGitHub/RealWorld_Functional_Programming/pop.csv"))
    let data = processLines(lines)
    let sum = float(calculateSum data)
    for (continent, population) in data do
        let percentage = int(float(population) / sum * 100.0)
        Console.WriteLine("{0,-18} - {1,8} ({2}%)",continent,population,percentage)

    let mainForm = new Form(Width = 620, Height = 450, Text = "Pie Chart")
    let menu = new ToolStrip()
    let btnOpen = new ToolStripButton("Open")
    let btnSave = new ToolStripButton("Save", Enabled = false) 
    ignore(menu.Items.Add(btnOpen)) 
    ignore(menu.Items.Add(btnSave))

    let boxChart = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)

    mainForm.Controls.Add(menu)
    mainForm.Controls.Add(boxChart)

    //[<STAThread>]
    do
        Application.Run(mainForm)

    0 // return an integer exit code


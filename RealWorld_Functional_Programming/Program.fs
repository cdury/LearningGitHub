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


(*for (continent, population) in data do
    let percentage = int(float(population) / sum * 100.0)
    Console.WriteLine("{0,-18} - {1,8} ({2}%)",continent,population,percentage)
*)

let mainForm = new Form(Width = 620, Height = 450, Text = "Pie Chart")
let menu = new ToolStrip()
let btnOpen = new ToolStripButton("Open")
let btnSave = new ToolStripButton("Save", Enabled = false) 
ignore(menu.Items.Add(btnOpen)) 
ignore(menu.Items.Add(btnSave))

let boxChart = new PictureBox(BackColor = Color.White, Dock = DockStyle.Fill, SizeMode = PictureBoxSizeMode.CenterImage)

mainForm.Controls.Add(menu)
mainForm.Controls.Add(boxChart)

let rnd = new Random()
let randomBrush() = 
    let r , g, b = rnd.Next(256), rnd.Next(256), rnd.Next(256)
    new SolidBrush(Color.FromArgb(r,g,b))

let drawPieSegment(gr:Graphics, title, startAngle, occupiedAngle) =
    let br = randomBrush()
    gr.FillPie(br, 170, 70, 260, 260, startAngle, occupiedAngle)
    br.Dispose()

let fnt = new Font("Times New Roman",11.0f)
let centerX, centerY = 300.0 , 200.0
let labelDistance = 150.0

let drawLabel(gr:Graphics, title, startAngle, angle) = 
    let lblAngle = float(startAngle + angle)/2.0
    let ra       = Math.PI * 2.0 * lblAngle/360.0
    let x        = centerX + labelDistance * cos(ra)
    let y        = centerY + labelDistance * sin(ra)
    let size     = gr.MeasureString(title, fnt)
    let rc       = new PointF(float32(x) - size.Width /2.0f, float32(y) - size.Height / 2.0f)
    gr.DrawString(title, fnt, Brushes.Black, new RectangleF(rc,size))

let drawStep(drawingFunc, gr:Graphics, sum, data) =
    let rec drawStepUtil(data, angleSoFar) =
        match data with
        | [] -> ()
        | [(title,value)] ->
            let angle = 360 - angleSoFar
            drawingFunc(gr, title, angleSoFar, angle)
        | (title,value)::tail ->
            let angle = int(float(value)/sum *360.0)
            drawingFunc(gr,title,angleSoFar, angle)
            drawStepUtil(tail,angleSoFar + angle)
    drawStepUtil(data,0)

let file = @"C:/Git/LearningGitHub/RealWorld_Functional_Programming/pop.csv"
let drawChart(file) =
    //let lines = List.ofSeq(File.ReadAllLines(@"/Users/christian/GitHub/LearningGitHub/RealWorld_Functional_Programming/pop.csv"))
    let lines = List.ofSeq(File.ReadAllLines(file))
    let data = processLines(lines)
    let sum = float(calculateSum data)
    let pieChart = new Bitmap(600,400)
    let gr       = Graphics.FromImage(pieChart)
    gr.Clear(Color.White)
    drawStep(drawPieSegment, gr, sum, data )
    drawStep(drawLabel, gr, sum, data)
    gr.Dispose()
    pieChart
    
let openAndDrawChart(e)=
    let pieChart = drawChart(file)
    boxChart.Image <- pieChart
    btnSave.Enabled <- true

let saveDrawing(e) =
    let dlg = new SaveFileDialog(Filter="PNG Files|*png")
    if (dlg.ShowDialog() = DialogResult.OK) then
        boxChart.Image.Save(dlg.FileName)
     
    

[<STAThread>]
do
    btnOpen.Click.Add(openAndDrawChart)
    btnSave.Click.Add(saveDrawing)
    Application.Run(mainForm)
    //ignore(Console.ReadKey())
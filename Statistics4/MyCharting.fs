module MyCharting

open System
open System.Windows.Forms
open System.Windows.Forms.DataVisualization.Charting

type ChartForm(xtitle, 
                ytitle, 
                ptype:SeriesChartType, 
                X : (float*float) seq, 
                Y:float seq,
                line:(float->float)) =
    inherit Form( Text="Regression" )

    let chart = new Chart(Dock=DockStyle.Fill)
    let area = new ChartArea(Name="Area1")
    //do area.AxisX.Minimum <- 0.0
    //do area.AxisX.Maximum <- 1.0
    //do area.AxisY.Minimum <- 0.0
    //do area.AxisY.Maximum <- 1.0

    let AllX = Seq.concat [X |> Seq.map fst; Y]

    let seriesX = new Series()
    do seriesX.ChartType <- ptype
    do seriesX.Color <- Drawing.Color.Green
    do X |> Seq.iter (seriesX.Points.AddXY >> ignore)
    do seriesX.ChartArea <- "Area1"
    do chart.Series.Add(seriesX)

    let seriesLine = new Series()
    do seriesLine.ChartType <- SeriesChartType.Line
    do seriesLine.Color <- Drawing.Color.Orange
    do [Seq.min AllX .. Seq.max AllX] 
        |> Seq.map (fun x -> (x, line x)) 
        |> Seq.iter (seriesLine.Points.AddXY >> ignore)
    do seriesLine.ChartArea <- "Area1"
    do chart.Series.Add(seriesLine)
    
    let seriesY = new Series()
    do seriesY.ChartType <- ptype
    do seriesY.Color <- Drawing.Color.Red
    do Y 
        |> Seq.map (fun y -> (y, line y)) 
        |> Seq.iter (seriesY.Points.AddXY >> ignore)
    do seriesY.ChartArea <- "Area1"    
    do chart.Series.Add(seriesY)

    do area.AxisX.Title <- xtitle
    do area.AxisY.Title <- ytitle
        
    do chart.ChartAreas.Add(area)
    do base.Controls.Add( chart )
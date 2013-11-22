namespace RCPSP

open System
open System.Drawing
open System.Windows.Forms
open System.Data

module ScheduleVisualisation =
    let show (ps:ProjectStructure) (sts:IntMap) (z:int->int->int) =
        let lblOffsetY = 500       

        let mainForm = new Form(Width = 1280, Height = 720, Text = "Ablaufplan")
        mainForm.StartPosition <- FormStartPosition.CenterScreen

        let addLbl text loc =
            let lbl = new Label()
            lbl.Text <- text
            lbl.Location <- loc
            mainForm.Controls.Add(lbl)

        let dgv = new DataGridView()

        let cell = new DataGridViewTextBoxCell()        

        let remZeroes str = if str = "0" then "" else str

        let initJobToColorMap() =
            let colMap = new System.Collections.Generic.Dictionary<int, Color>()
            colMap.Add(0, Color.White)
            for j in ps.Jobs do
                let rval () = Utils.rand 20 255
                colMap.Add(j, Color.FromArgb(rval(), rval(), rval()))
            colMap

        let capLbl = new Label()
        let updateCapLbl cap =
            capLbl.Text <- "Capacity: " + string cap
        updateCapLbl (ps.Capacities 1)
        capLbl.Location <- new Point(10, lblOffsetY+90)
        mainForm.Controls.Add(capLbl)
        
        let colMap = initJobToColorMap()
        let setCell (i:int) (j:int) (v:int) =
                let cell = dgv.[j,i]
                cell.Value <- remZeroes (v.ToString())
                cell.Style.BackColor <- colMap.[v]

        let addZrtRow r =
            let rcount = dgv.Rows.Count
            let dgvr = new DataGridViewRow()
            dgvr.Height <- 20
            dgvr.HeaderCell.Value <- "zrt"
            dgv.Rows.Add(dgvr) |> ignore
            for t in ps.TimeHorizon do
                let cell = dgv.[t-1, rcount-1]
                cell.Value <- z r t

        let updateGridForRes r =
            dgv.Columns.Clear()
            dgv.Rows.Clear()

            let grid = ps.ScheduleToGrid sts r

            let nrows = Array2D.length1 grid
            let ncols = Array2D.length2 grid

            for t in [1..ncols] do
                let dgvc = new DataGridViewColumn()
                dgvc.Width <- 20
                dgvc.CellTemplate <- cell
                dgvc.HeaderText <- t.ToString()
                dgv.Columns.Add(dgvc) |> ignore

            for k in [1..nrows] do            
                let dgvr = new DataGridViewRow()
                dgvr.Height <- 12
                dgvr.HeaderCell.Value <- (nrows-k+1).ToString()
                dgv.Rows.Add(dgvr) |> ignore

            Array2D.iteri setCell grid
            addZrtRow r
            dgv.Refresh()

            updateCapLbl (ps.Capacities r)

        updateGridForRes 1

        dgv.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left
        dgv.AutoSize <- true
        dgv.CellBorderStyle <- DataGridViewCellBorderStyle.None
        dgv.ScrollBars <- ScrollBars.Both

        mainForm.Controls.Add(dgv)
        mainForm.HorizontalScroll.Enabled <- true
        mainForm.VerticalScroll.Enabled <- true
        mainForm.AutoScroll <- true

        addLbl "Selected resource" (new Point(10, lblOffsetY))

        let resCb = new ComboBox()
        resCb.DataSource <- Array.ofSeq ps.Resources
        resCb.Location <- new Point(10, lblOffsetY+30)
        resCb.SelectedValueChanged.Add(fun v -> updateGridForRes (resCb.SelectedIndex+1))
        mainForm.Controls.Add(resCb)

        addLbl ("Makespan: " + sts.[sts.Keys.Count].ToString()) (new Point(10, lblOffsetY+60))
        
        addLbl ("Horizon: " + string ps.TimeHorizon.Length) (new Point(10, lblOffsetY+120))

        Application.Run(mainForm)
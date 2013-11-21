namespace RCPSP

open System
open System.Drawing
open System.Windows.Forms
open System.Data

module ScheduleVisualisation =
    let show (ps:ProjectStructure) (sts:IntMap) =
        let mainForm = new Form(Width = 640, Height = 450, Text = "Ablaufplan")

        let dgv = new DataGridView()

        let cell = new DataGridViewTextBoxCell()        

        let remZeroes str = if str = "0" then "" else str

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

            Array2D.iteri (fun i j v -> dgv.[j,i].Value <- remZeroes (grid.[i,j].ToString())) grid
            dgv.Refresh()

        updateGridForRes 1

        dgv.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left
        dgv.AutoSize <- true
        dgv.CellBorderStyle <- DataGridViewCellBorderStyle.None
        dgv.ScrollBars <- ScrollBars.Both

        mainForm.Controls.Add(dgv)
        mainForm.HorizontalScroll.Enabled <- true
        mainForm.VerticalScroll.Enabled <- true

        let resLbl = new Label()
        resLbl.Text <- "Selected resource"
        resLbl.Location <- new Point(10, 290)
        mainForm.Controls.Add(resLbl)

        let resCb = new ComboBox()
        resCb.DataSource <- Array.ofSeq ps.Resources
        resCb.Location <- new Point(10, 320)
        resCb.SelectedValueChanged.Add(fun v -> updateGridForRes (resCb.SelectedIndex+1))
        mainForm.Controls.Add(resCb)

        let msLbl = new Label()
        msLbl.Text <- "Makespan: " + sts.[sts.Keys.Count].ToString()
        msLbl.Location <- new Point(10, 350)
        mainForm.Controls.Add(msLbl)

        Application.Run(mainForm)
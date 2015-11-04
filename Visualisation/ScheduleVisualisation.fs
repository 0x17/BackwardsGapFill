namespace RCPSP

open System
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

open Utils

module ScheduleVisualisation =
    let private saveViewToPng (view:Control) filename =
        let bmp = new Bitmap (view.Width, view.Height)
        view.DrawToBitmap(bmp, Rectangle(Point.Empty, bmp.Size))
        bmp.Save (filename+".png")

    let saveViews prefix views =
        Seq.iteri (fun i view -> saveViewToPng view (prefix+string(i+1))) views
        
    let private show caption (ps:ProjectStructure) (sts:Map<int,int>) =
        let lblOffsetY = 500

        let mainForm = new Form (Width = 1280, Height = 800, Text = "Ablaufplan - " + caption, StartPosition=FormStartPosition.CenterScreen)

        let z = ps.NeededOCForSchedule sts

        let addLbl text loc =
            let lbl = new Label (Text=text, Location=loc, Size=Size(200,20))
            mainForm.Controls.Add lbl

        let dgv = new DataGridView ()

        let cell = new DataGridViewTextBoxCell ()        

        let remZeroes str = if str = "0" then "" else str

        let initJobToColorMap () =
            let r = Random 23
            let colMap = Dictionary<int, Color> ()
            colMap.Add (0, Color.White)
            for j in ps.Jobs do
                let rval () = r.Next (20, 256)
                colMap.Add(j, Color.FromArgb(rval (), rval (), rval ()))
            colMap

        let capLbl = new Label ()
        let updateCapLbl cap =
            capLbl.Text <- "Capacity: " + string cap
        updateCapLbl (ps.Capacities 1)
        capLbl.Location <- Point (10, lblOffsetY+90)
        mainForm.Controls.Add capLbl
        
        let colMap = initJobToColorMap()
        let setCell (i:int) (j:int) (v:int) =
                let cell = dgv.[j,i]
                cell.Value <- remZeroes (v.ToString ())
                cell.Style.BackColor <- colMap.[v]

        let addZrtRow r =
            let rcount = dgv.Rows.Count
            let dgvr = new DataGridViewRow ()
            dgvr.Height <- 20
            dgvr.HeaderCell.Value <- "zrt"
            dgv.Rows.Add dgvr |> ignore
            for t in ps.TimeHorizon do
                let cell = dgv.[t-1, rcount-1]
                cell.Value <- z r t

        let scheduleToGrid r =
            let nrows = ps.Capacities r + ps.ZMax r
            let grid = Array2D.zeroCreate nrows ps.TimeHorizon.Length
            for t in ps.TimeHorizon do
                let actJobs = ps.ActiveInPeriodSet sts t
                let mutable colCtr = dec nrows
                for j in actJobs do
                    for k in 1..ps.Demands j r do
                        Array2D.set grid colCtr (dec t) j
                        colCtr <- dec colCtr
            grid

        let updateGridForRes r =
            dgv.Columns.Clear ()
            dgv.Rows.Clear ()

            let grid = scheduleToGrid r

            let nrows = Array2D.length1 grid
            let ncols = Array2D.length2 grid

            for t in 1..ncols do
                let dgvc = new DataGridViewColumn ()
                dgvc.Width <- 20
                dgvc.CellTemplate <- cell
                dgvc.HeaderText <- t.ToString ()
                dgv.Columns.Add dgvc |> ignore

            for k in 1..nrows do            
                let dgvr = new DataGridViewRow ()
                dgvr.Height <- 12
                dgvr.HeaderCell.Value <- (nrows-k+1).ToString ()
                dgv.Rows.Add dgvr |> ignore

            Array2D.iteri setCell grid
            addZrtRow r
            dgv.Refresh ()

            updateCapLbl (ps.Capacities r)

        updateGridForRes 1

        dgv.Anchor <- AnchorStyles.Top ||| AnchorStyles.Left
        dgv.AutoSize <- true
        dgv.CellBorderStyle <- DataGridViewCellBorderStyle.None
        dgv.ScrollBars <- ScrollBars.Both

        mainForm.Controls.Add dgv
        mainForm.HorizontalScroll.Enabled <- true
        mainForm.VerticalScroll.Enabled <- true
        mainForm.AutoScroll <- true

        addLbl "Selected resource" (Point (10, lblOffsetY))

        let resCb = new ComboBox ()
        resCb.DataSource <- Array.ofSeq ps.Resources
        resCb.Location <- Point (10, lblOffsetY+30)
        resCb.SelectedValueChanged.Add(fun _ -> updateGridForRes (resCb.SelectedIndex+1))
        mainForm.Controls.Add resCb

        addLbl ("Makespan: " + sts.[Seq.length (keys sts)].ToString ()) (Point (10, lblOffsetY+60))
        
        addLbl ("Horizon: " + string ps.TimeHorizon.Length) (Point (10, lblOffsetY+120))

        let sumOc = ps.Resources >< ps.TimeHorizon |> Seq.sumBy (fun (r,t) -> float (z r t) * (ps.Kappa r))
        addLbl ("Total OC costs: " + string sumOc) (Point(10, lblOffsetY+150))

        let profit = ps.Profit sts
        addLbl ("Profit: " + string profit) (Point(10, lblOffsetY+180))

        mainForm.Closed.Add(fun _ -> Application.Exit ())
        mainForm.Show ()

        dgv

    let showSchedules data =
        //Seq.map (fun (caption,ps,sts) -> show caption ps sts) data |> saveViews "schedule"
        Seq.iter (fun (caption,ps,sts) -> show caption ps sts |> ignore) data
        System.Windows.Forms.Application.Run ()

    let showSchedule caption ps sts = showSchedules [(caption, ps, sts)]

    let showPipe ps sts =
        showSchedules [("Schedule", ps, sts)]
        sts

    let exactSolvePrompt () =
        let f = new Form(AutoSize=true, Text="File selection", StartPosition=FormStartPosition.CenterScreen)

        let projFn = ref ""

        let fileDialog () =
            let ofd = new OpenFileDialog()
            if ofd.ShowDialog() = DialogResult.OK then ofd.FileName else ""

        let batchAdd controls =
            Seq.iter (fun control -> f.Controls.Add(control)) controls

        let (xOffset, yOffset) = (10, 10)

        let projFileLbl = new Label(Text = "Project .SM file:", Size = Size(300,30), Location = Point(xOffset, yOffset))
        let projFileSelBtn = new Button(Text = "Select", Location = Point(xOffset+300, yOffset))
        projFileSelBtn.Click.Add(fun ev -> projFn := fileDialog ()
                                           projFileLbl.Text <- !projFn)

        let doSolveBtn = new Button(Text = "Solve and visualize", Location = Point(xOffset, 100), Size = Size(200, 30))
        doSolveBtn.Click.Add(fun ev ->
            if (!projFn).Length > 0 then
                let ps = PSPLibParser.parse !projFn
                let sts = (fst3 <| GamsSolver.solve ps)
                show ("Schedule") ps sts |> ignore)

        batchAdd [projFileLbl :> Control;
                  projFileSelBtn :> Control;
                  doSolveBtn :> Control]

        f.Show ()
        f.Closed.Add(fun _ -> Application.Exit ())
        System.Windows.Forms.Application.Run ()

    let fileSelectionPrompt () =
        let f = new Form(AutoSize=true, Text="File selection", StartPosition=FormStartPosition.CenterScreen)

        let projFn = ref ""
        let stsFn = ref ""

        let fileDialog () =
            let ofd = new OpenFileDialog()
            if ofd.ShowDialog() = DialogResult.OK then ofd.FileName else ""

        let batchAdd controls =
            Seq.iter (fun control -> f.Controls.Add(control)) controls

        let (xOffset, yOffset) = (10, 10)

        let projFileLbl = new Label(Text = "Project .SM file:", Size = Size(300,30), Location = Point(xOffset, yOffset))
        let projFileSelBtn = new Button(Text = "Select", Location = Point(xOffset+300, yOffset))
        projFileSelBtn.Click.Add(fun ev -> projFn := fileDialog ()
                                           projFileLbl.Text <- !projFn)
        let stsFileLbl = new Label(Text = "Schedule file:", Size = Size(300,30), Location = Point(xOffset, yOffset+40))
        let stsFileSelBtn = new Button(Text = "Select", Location = Point(xOffset+300, yOffset+40))
        stsFileSelBtn.Click.Add(fun ev -> stsFn := fileDialog ()
                                          stsFileLbl.Text <- !stsFn)

        let doVisBtn = new Button(Text = "Visualize", Location = Point(xOffset, 100))
        doVisBtn.Click.Add(fun ev ->
            if (!projFn).Length > 0 && (!stsFn).Length > 0 then
                show ("Schedule " + !stsFn) (PSPLibParser.parse !projFn) (Serialization.slurpMap !stsFn) |> ignore)

        batchAdd [projFileLbl :> Control;
                  projFileSelBtn :> Control;
                  stsFileLbl :> Control; 
                  stsFileSelBtn :> Control;
                  doVisBtn :> Control]

        f.Show ()
        f.Closed.Add(fun _ -> Application.Exit ())
        System.Windows.Forms.Application.Run ()
    
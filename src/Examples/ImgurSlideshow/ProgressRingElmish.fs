namespace ImgurSlideshow
open System
open Avalonia
open Elmish
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Controls.Presenters
open Avalonia.Controls.Primitives
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Threading
open Avalonia.FuncUI
open Avalonia.FuncUI.Types

module ProgressRingElmish =
    type Model = {
        timer: DispatcherTimer
        angle: float
        rectWidth: float
        radius: float
        center: Point
    }

    type Msg = Tick
             | SizeChange of Size

    let init radius =
        { timer = DispatcherTimer(Interval = TimeSpan.FromMilliseconds(1000.0/60.0))
          angle = 3.0
          radius = radius
          rectWidth = radius / 3.0
          center = Point(100.0, 100.0)
        }, Cmd.none

    let subscribe initial =
        let sub dispatch =
           initial.timer.Tick.Add (fun state -> dispatch Tick)
           initial.timer.Start()
        Cmd.ofSub sub

    let update msg model =
        match msg with
        | Tick         -> { model with angle = model.angle + 360.0 / 120.0 }, Cmd.none
        | SizeChange s -> { model with center = Point(s.Width / 2.0, s.Height / 2.0)}, Cmd.none

    let makeRect (center : Point) rectWidth radius dispatch alpha =
        let w    = rectWidth
        let h    = rectWidth / 2.0
        let left = center.X + radius * cos alpha - w / 2.0
        let top  = center.Y + radius * sin alpha - h / 2.0
        Views.rectangle [
            Attrs.width w
            Attrs.height h
            Attrs.renderTransform (RotateTransform (Angle = 180.0 * (alpha / Math.PI)))
            Attrs.canvas_left left
            Attrs.canvas_top top
            Attrs.fill Brushes.Black
        ]

    let view state dispatch attrs =
        let step   = 2.0 * Math.PI / 10.0
        let mkr    = makeRect state.center state.rectWidth state.radius dispatch
        let rects = seq {for i in 1..10 -> mkr ((float i) * step)} |> Seq.toList
        let origin = RelativePoint(state.center.X, state.center.Y, RelativeUnit.Absolute)
        Views.canvas ([
            Attrs.children rects
            Attrs.renderTransform (RotateTransform(Angle = state.angle))
            Attrs.renderTransformOrigin origin
            Attrs.onLayoutUpdated (fun sender args -> dispatch (SizeChange (sender :?> Control).Bounds.Size))
        ] @ attrs)

namespace ImgurSlideshow

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Controls.Presenters
open Avalonia.Controls.Primitives
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Threading
open Avalonia.FuncUI
open Avalonia.FuncUI.Types


type ProgressRing() as this =
    inherit UserControl()

    let RadiusProperty = AvaloniaProperty.Register<ProgressRing, float>("Radius", 15.0, inherits=true)

    let _timer = DispatcherTimer(Interval = TimeSpan.FromMilliseconds(1000.0 / 60.0))

    let mutable _alpha = 0.0

    do this.InitializeComponent()
       let onTimer (args: EventArgs) =
            match this.Content :?> Canvas with
            | null -> ()
            | canvas ->
                let tr = RotateTransform(Angle = _alpha)
                _alpha <- _alpha + 360.0 / 120.0
                canvas.RenderTransform <- tr
                let s = canvas.Bounds.Size
                canvas.RenderTransformOrigin <- RelativePoint(s.Width / 2.0,
                                                              s.Height / 2.0,
                                                              RelativeUnit.Absolute)
       _timer.Tick.Add(onTimer)
       _timer.Start()
       this.LayoutUpdated.Add this.Populate


    member public this.Radius
      with get ()   = this.GetValue RadiusProperty
      and set value = this.SetValue (RadiusProperty, value)

    member this.MakeRect (alpha, size, radius) =
        let canvas = this.Content :?> Canvas
        let r = Rectangle(Width = size, Height = size / 2.0, Fill = this.Foreground)
        r.RenderTransform <- RotateTransform(Angle =  180.0 * (alpha / Math.PI))

        let hs = canvas.Bounds.Size / 2.0
        let left = hs.Width + radius * cos alpha - r.Width / 2.0
        let top = hs.Height + radius * sin alpha - r.Height / 2.0
        Canvas.SetLeft(r, left)
        Canvas.SetTop(r, top)
        r

    member this.Populate _ =
        let canvas = this.Content :?> Canvas
        canvas.Children.Clear()
        for i in 1..10 do
            let step = 2.0 * Math.PI / 10.0
            let r = this.MakeRect ((float i)*step, this.Radius / 3.0, this.Radius)
            canvas.Children.Add(r)


    member this.InitializeComponent () =
        printfn "InitializeComponent called!!"
        AvaloniaXamlLoader.Load this

[<AutoOpen>]
module ProgressRingDSLExtensions =
  type Views with
      static member progressRing (attrs: TypedAttr<ProgressRing> list): View =
              Views.create<ProgressRing>(attrs)


  type Attrs with

      static member inline radius<'T when 'T : (member set_Radius : float -> unit)>(value: float) : TypedAttr<'T> =
          TypedAttr<_>.Property { Name = "Radius"; Value = value }

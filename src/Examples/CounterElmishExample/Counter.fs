namespace CounterElmishSample

open Avalonia.Controls
open Avalonia.Media
open Avalonia.FuncUI.Types
open Avalonia.FuncUI
open Avalonia.Layout
open Avalonia.Controls.Presenters
open Avalonia.Controls.Templates
open Avalonia
open Avalonia.Data
open Avalonia.Styling

type CustomControl() =
    inherit Control()

    member val Text: string = "" with get, set

[<AutoOpen>]
module ViewExt =
    type Views with
        static member customControl (attrs: TypedAttr<CustomControl> list): View =
            Views.create<CustomControl>(attrs)

module Counter =

    type CounterState = {
        count : int
    }

    let init = {
        count = 0
    }

    type Msg =
    | Increment
    | Decrement

    let update (msg: Msg) (state: CounterState) : CounterState =
        match msg with
        | Increment -> { state with count =  state.count + 1 }
        | Decrement -> { state with count =  state.count - 1 }
    
    let view (state: CounterState) (dispatch): View =
        Views.dockpanel [
            Attrs.styles <| Styles.Create [
                Style.Create((fun s -> s.OfType<Button>().Class(".round").Template().OfType<ContentPresenter>()), [
                    Setter.Create(Border.CornerRadiusProperty, Avalonia.CornerRadius(100.))
                ])
                Style.Create((fun s -> s.Is<Control>().Class(".margin-x")), [
                    Setter.Create(Control.MarginProperty, Avalonia.Thickness(5.))
                ])
            ]
            Attrs.children [
                Views.button [
                    Attrs.dockPanel_dock Dock.Bottom
                    Attrs.onClick (fun sender args -> dispatch Decrement)
                    Attrs.content "-"
                    Attrs.classes [ ".round"; ".margin-x" ]   
                ]
                Views.button [
                    Attrs.dockPanel_dock Dock.Bottom
                    Attrs.onClick (fun sender args -> dispatch Increment)
                    Attrs.margin 5.
                    Attrs.content "+"
                    Attrs.classes [ ".round"; ".margin-x" ]   
                ]
                Views.textBlock [
                    Attrs.dockPanel_dock Dock.Top
                    Attrs.fontSize 48.0
                    Attrs.verticalAlignment VerticalAlignment.Center
                    Attrs.horizontalAlignment HorizontalAlignment.Center
                    Attrs.text (string state.count)
                ]
            ]
        ]       

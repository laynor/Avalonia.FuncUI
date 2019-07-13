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
            Attrs.children [
                Views.button [
                    Attrs.dockPanel_dock Dock.Bottom
                    Attrs.onClick (fun sender args -> dispatch Decrement)
                    Attrs.content "-"
                ]
                Views.button [
                    Attrs.dockPanel_dock Dock.Bottom
                    Attrs.onClick (fun sender args ->
                        let btn = sender :?> IStyledElement
                        btn.NotifyResourcesChanged(ResourcesChangedEventArgs())

                        dispatch Increment
                    )
                    Attrs.margin 5.
                    Attrs.content "+"
                    Attrs.styles <| (
                        let styles = Styles()

                        let style = Style(fun s -> s.OfType<Button>().Template().OfType<ContentPresenter>())
                        let setter = Setter(Border.CornerRadiusProperty, Avalonia.CornerRadius(10.))
                        style.Setters.Add setter
                        
                        //let style = Style(fun s -> s.OfType<Button>())
                        //let setter = Setter(Button.BackgroundProperty, Brushes.Green)
                        //style.Setters.Add setter

                        styles.Add style
                        styles
                    )
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

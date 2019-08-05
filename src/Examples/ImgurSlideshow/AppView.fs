namespace ImgurSlideshow

open Avalonia
open Elmish

open Avalonia.Controls
// open Avalonia.Controls.Presenters
open Avalonia.Media
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Styling
open Avalonia.FuncUI.Elmish

module Sty =
    let ofType<'T when 'T :> IStyleable> (sel:Selector): Selector =
        sel.OfType<'T>()

    let template (sel:Selector) =
        sel.Template()

    let withClass cls (sel:Selector) =
        sel.Class(cls)

    let setter (prop:AvaloniaProperty) (value:obj):Setter =
        Setter.Create(prop, value)


module ImgurSlideshowView =

    // The model holds data that you want to keep track of while the application is running
    type ImgurSlideshowState = {
        query:       string
        images:      Imgur.Image list
        current:     int option
        dispatch:    Dispatcher option
        imageSource: Imaging.Bitmap
        loading:     bool
        ringModel:   ProgressRingElmish.Model
    }
    and Dispatcher = Msg -> unit
    and Msg = QueryTextChanged of string
            | SeePrevious
            | SeeNext
            | StartSearch
            | SearchPerformed of Imgur.Image list * int option
            | SetImageSource of Imaging.Bitmap
            | Test of string
            | Ring of ProgressRingElmish.Msg

    //The initial state of of the application
    let init () =
        let ringState, ringCmd = ProgressRingElmish.init 50.0
        { loading     = false
          query       = "cats"
          images      = []
          current     = None
          dispatch    = None
          imageSource = null
          ringModel   = ringState },
        Cmd.batch [ Cmd.map Ring ringCmd ]

    // The Msg type defines what events/actions can occur while the application is running
    // the state of the application changes *only* in reaction to these events
    let showNthImage state n =
        match n with
        | Some(m) when m >=0 && m <= List.length state.images ->
            { state with current = n; loading = true }

        | _ -> failwith (sprintf "Cannot show image #%A" n)


    // The update function computes the next state of the application based on the current state and the incoming messages

    let testfn () = "antani"

    let searchImagesAsync query = async {
        match! Imgur.startSearch query with
        | []   -> return [], None
        | imgs -> return imgs, Some 0
    }

    let getImageSourceAsync img =
        async {
            match img with
            | None   -> return null
            | Some i ->
                match! (Imgur.getImage i) with
                | Some p  ->
                    try
                      return new Imaging.Bitmap(p)
                    with
                      | ex -> printfn "Exception!!!! %A" ex
                              return null
                | None -> return null
        }

    let update (msg: Msg) (state: ImgurSlideshowState) : ImgurSlideshowState * Cmd<Msg> =
        try
            printfn "Update"
            let displayImageCommand s =
                Cmd.OfAsync.perform getImageSourceAsync
                                    (Option.map (fun o -> List.item o s.images) s.current)
                                    SetImageSource

            match msg with
            | QueryTextChanged(s)              -> { state with query = s },
                                                  Cmd.none

            | SeeNext                          -> let s = showNthImage state (Option.map ((+) 1) state.current)
                                                  s, displayImageCommand s

            | SeePrevious                      -> let s = showNthImage state (Option.map (fun x -> x - 1) state.current)
                                                  s, displayImageCommand s

            | StartSearch                      -> { state with loading = true },
                                                  Cmd.OfAsync.perform searchImagesAsync state.query SearchPerformed

            | SearchPerformed(images, current) -> let s = {state with images = images; current=current }
                                                  s, displayImageCommand s
            | SetImageSource source            -> { state with imageSource = source; loading = false }, Cmd.none
            | Ring msg'                        -> let model, cmd = ProgressRingElmish.update msg' state.ringModel
                                                  { state with ringModel = model }, Cmd.map Ring cmd
            | Test s                           -> printfn "Test %A" s; state,
                                                  Cmd.none

        with
            | ex ->
                printfn "EXCEPTION: %A" ex
                state, Cmd.none


    let columnDefinitions = ColumnDefinitions "50, 1*, 50"
    let rowDefinitions    = RowDefinitions    "auto, 1*, 50, 1*"

    let subscription (state:ImgurSlideshowState) =
        Cmd.batch [
            Cmd.map Ring (ProgressRingElmish.subscribe state.ringModel)
        ]

    // The view function returns the view of the application depending on its current state. Messages can be passed to the dispatch function.
    let view (state: ImgurSlideshowState) (dispatch): View =

        let onQueryPropertyChange _ (args:Avalonia.AvaloniaPropertyChangedEventArgs) =
            match args.Property.Name with
            | "Text" -> dispatch (QueryTextChanged (args.NewValue :?> string))
            | _ -> ()

        Views.grid [
            Attrs.styles <| Styles.Create [
                // Style.Create((fun s -> s.OfType<Button>().Class(".round").Template().OfType<Presenters.ContentPresenter>()), [
                //     Setter.Create(Border.CornerRadiusProperty, Avalonia.CornerRadius(20.))
                // ])

                Style.Create((fun s -> s |> Sty.ofType<Button> |> Sty.withClass ".round" |> Sty.template |> Sty.ofType<Presenters.ContentPresenter>), [
                    Sty.setter Border.CornerRadiusProperty (Avalonia.CornerRadius 20.0)
                ])

                Style.Create((fun s -> s.OfType<Button>().Class(".round")), [
                    Sty.setter Button.BackgroundProperty Brushes.Yellow
                ])

            ]
            Attrs.columnDefinitions columnDefinitions
            Attrs.rowDefinitions rowDefinitions
            Attrs.children [
                Views.textBox [
                    Attrs.grid_row 0
                    Attrs.grid_column 0
                    Attrs.grid_columnSpan 2
                    Attrs.text state.query
                    Attrs.onPropertyChanged onQueryPropertyChange
                    Attrs.onKeyDown (fun sender args -> match args.Key with
                                                        | Avalonia.Input.Key.Enter -> dispatch StartSearch
                                                        | _ -> ())
                ]

                Views.button [
                    Attrs.grid_row 0
                    Attrs.grid_column 2
                    Attrs.content "Search"
                    Attrs.onClick (fun sender args -> dispatch StartSearch)
                ]

                Views.button [
                    Attrs.grid_row 2
                    Attrs.grid_column 0
                    Attrs.fontSize 32.0
                    Attrs.content "<"
                    Attrs.classes [".round"]
                    Attrs.isEnabled (match state.current with
                                     | Some n when n > 0 -> true
                                     | _ -> false)
                    Attrs.onClick (fun sender args -> dispatch SeePrevious)
                ]

                Views.button [
                    Attrs.grid_row 2
                    Attrs.grid_column 2
                    Attrs.fontSize 32.0
                    Attrs.content ">"
                    Attrs.classes [".round"]
                    Attrs.isEnabled (match state.current with
                                     | Some n when n < List.length (state.images) - 1 -> true
                                     | _ -> false)
                    Attrs.onClick (fun sender args -> dispatch SeeNext)
                ]

                Views.image [
                    Attrs.grid_row 1
                    Attrs.grid_rowSpan 3
                    Attrs.grid_column 1
                    Attrs.source state.imageSource
                ]
                // Views.progressRing [
                //     Attrs.grid_row 1
                //     Attrs.grid_rowSpan 3
                //     Attrs.grid_column 1
                //     Attrs.radius 20.0
                //     Attrs.isVisible state.loading
                // ]
                ProgressRingElmish.view state.ringModel (Ring >> dispatch) [
                    Attrs.grid_row 1
                    Attrs.grid_rowSpan 3
                    Attrs.grid_column 1
                    Attrs.isVisible state.loading
                ]
            ]
        ]

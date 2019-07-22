namespace ImgurSlideshow

open Avalonia.Controls
open Avalonia.Media
open Avalonia.FuncUI.Types
open Avalonia.FuncUI
open Avalonia.Layout

module ImgurSlideshowView =

    // The model holds data that you want to keep track of while the application is running
    type ImgurSlideshowState = {
        query:   string
        images:  Imgur.Image list
        current: int option
    }

    //The initial state of of the application
    let initialState = {
        query   = "cats"
        images  = []
        current = None
    }

    // The Msg type defines what events/actions can occur while the application is running
    // the state of the application changes *only* in reaction to these events
    type Msg = QueryTextChanged of string
             | SeePrevious
             | SeeNext
             | DoSearch

    let doSearch state =
        let images = Imgur.searchImages state.query
        let current = if List.isEmpty images then None else Some(0)
        { state with images  = images
                     current = current }

    let showNthImage state n =
        match n with
        | Some(m) when m >=0 && m <= List.length state.images ->
            { state with current = n }
        | _ -> failwith (sprintf "Cannot show image #%A" n)

    // The update function computes the next state of the application based on the current state and the incoming messages
    let update (msg: Msg) (state: ImgurSlideshowState) : ImgurSlideshowState =
        try
            printfn "Update"
            match msg with
            | QueryTextChanged(s) -> { state with query = s }
            | SeeNext             -> showNthImage state (Option.map ((+) 1) state.current)
            | SeePrevious         -> showNthImage state (Option.map (fun x -> x - 1) state.current)
            | DoSearch            -> doSearch state
        with
            | ex ->
                printfn "EXCEPTION: %A" ex
                state


    let columnDefinitions = ColumnDefinitions "50, 1*, 50"
    let rowDefinitions    = RowDefinitions "auto, 1*, 50, 1*"

    let imageSource state =
        let source = match state.current with
                     | None -> "/home/alessandro/Pictures/gianfinocchio.jpg"
                     | Some(n) -> Imgur.getImage (List.item n state.images)
        new Imaging.Bitmap( source )


    // The view function returns the view of the application depending on its current state. Messages can be passed to the dispatch function.
    let view (state: ImgurSlideshowState) (dispatch): View =

        let onQueryPropertyChange _ (args:Avalonia.AvaloniaPropertyChangedEventArgs) =
            match args.Property.Name with
            | "Text" -> dispatch (QueryTextChanged (args.NewValue :?> string))
            | _ -> ()

        Views.grid [
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
                                                        | Avalonia.Input.Key.Enter -> dispatch DoSearch
                                                        | _ -> ())
                ]

                Views.button [
                    Attrs.grid_row 0
                    Attrs.grid_column 2
                    Attrs.content "Search"
                    Attrs.onClick (fun sender args -> dispatch DoSearch)
                ]

                Views.button [
                    Attrs.grid_row 2
                    Attrs.grid_column 0
                    Attrs.fontSize 32.0
                    Attrs.content "<"
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
                    Attrs.isEnabled (match state.current with
                                     | Some n when n < List.length (state.images) - 1 -> true
                                     | _ -> false)
                    Attrs.onClick (fun sender args -> dispatch SeeNext)
                ]

                Views.image [
                    Attrs.grid_row 1
                    Attrs.grid_rowSpan 3
                    Attrs.grid_column 1
                    Attrs.source (imageSource state)
                ]
            ]
        ]

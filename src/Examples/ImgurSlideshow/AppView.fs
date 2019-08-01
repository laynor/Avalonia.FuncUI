﻿namespace ImgurSlideshow

open Avalonia.Controls
open Avalonia.Media
open Avalonia.FuncUI.Types
open Elmish
open Avalonia.FuncUI
open Avalonia.Layout
open Avalonia.FuncUI.Elmish

module ImgurSlideshowView =

    // The model holds data that you want to keep track of while the application is running
    type ImgurSlideshowState = {
        query:       string
        images:      Imgur.Image list
        current:     int option
        dispatch:    Dispatcher option
        imageSource: Imaging.Bitmap
    }
    and Dispatcher = Msg -> unit
    and Msg = QueryTextChanged of string
            | SeePrevious
            | SeeNext
            | StartSearch
            | SearchPerformed of Imgur.Image list * int option
            | SetImageSource of Imaging.Bitmap
            | Test of string

    //The initial state of of the application
    let initialState = {
        query       = "cats"
        images      = []
        current     = None
        dispatch    = None
        imageSource = null
    }

    // The Msg type defines what events/actions can occur while the application is running
    // the state of the application changes *only* in reaction to these events
    let showNthImage state n =
        match n with
        | Some(m) when m >=0 && m <= List.length state.images ->
            { state with current = n }

        | _ -> failwith (sprintf "Cannot show image #%A" n)


    // The update function computes the next state of the application based on the current state and the incoming messages

    let testfn () = "antani"

    let searchImagesAsync query = async {
        let! images = Imgur.startSearch query
        let current = match images with
                      | [] -> None
                      | _  -> Some(0)
        return images, current
    }

    let getImageSourceAsync img =
        async {
            match img with
            | None   -> return null
            | Some i ->
                let! path = Imgur.getImage i
                match path with
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

            | StartSearch                      -> state,
                                                  Cmd.OfAsync.perform searchImagesAsync state.query SearchPerformed

            | SearchPerformed(images, current) -> let s = {state with images = images; current=current}
                                                  s, displayImageCommand s
            | SetImageSource source            -> { state with imageSource = source }, Cmd.none
            | Test s                           -> printfn "Test %A" s; state,
                                                  Cmd.none

        with
            | ex ->
                printfn "EXCEPTION: %A" ex
                state, Cmd.none


    let columnDefinitions = ColumnDefinitions "50, 1*, 50"
    let rowDefinitions    = RowDefinitions    "auto, 1*, 50, 1*"


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
                    Attrs.source state.imageSource
                ]
                Views.progressRing [
                    Attrs.grid_row 1
                    Attrs.grid_rowSpan 3
                    Attrs.grid_column 1
                    Attrs.radius 20.0
                ]
            ]
        ]

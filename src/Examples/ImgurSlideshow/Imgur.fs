namespace ImgurSlideshow
open FSharp.Data
open FSharp.Json

// Client id: 7d5e7351a586413
module Imgur =
    type GallerySearchResponse = { data: Gallery list }
    and Gallery = { images: Image list option }
    and Image = { title: string option

                  [<JsonField("type")>]
                  typ: string

                  [<JsonField("link")>]
                  url: string }

    let auth = "Client-ID 7d5e7351a586413"

    let reqSearchGalleries query =
        let formatUrl sort window page =
            sprintf "https://api.imgur.com/3/gallery/search/%A/%A/%A" sort window page
        Http.RequestString ((formatUrl "" "" ""),
                            query = ["q", query],
                            headers = ["Authorization", auth])
        |> Json.deserialize<GallerySearchResponse>

    let searchImages query =
        let response = reqSearchGalleries query
        response.data
        |> List.collect (fun g -> match g.images with
                                  | Some(l) -> l
                                  | _ -> [])
        |> List.filter (fun img -> img.typ = "image/jpeg")

    let getImgPath img =
        let substChars = function
                       | '/' -> '_'
                       | c -> c

        let filename = img.url
                       |> Seq.map substChars
                       |> System.String.Concat

        sprintf "/tmp/ImgurSlideshow/images/%A" filename

    let downloadBinary url path =
        let req = Http.Request (url, headers = [ "Authorization", auth ])
        match req.Body with
        | Binary bytes ->
            System.IO.File.WriteAllBytes(path, bytes)
            path
        | Text _ -> failwith "Received text"

    let prepareStorage () =
        System.IO.Directory.CreateDirectory "/tmp/ImgurSlideshow/images" |> ignore

    let getImage img =
        let imgPath = getImgPath img
        if System.IO.File.Exists imgPath
        then imgPath
        else (downloadBinary img.url imgPath)

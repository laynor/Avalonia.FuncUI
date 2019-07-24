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

    let formatSearchGalleryUrl sort window page =
        sprintf "https://api.imgur.com/3/gallery/search/%A/%A/%A" sort window page

    let startSearch query =
        let url = formatSearchGalleryUrl "" "" ""
        async {
            let! response = Http.AsyncRequestString (url, query = ["q", query], headers = ["Authorization", auth])
            let json = response |> Json.deserialize<GallerySearchResponse>
            return json.data
            |> List.collect (fun g -> match g.images with
                                      | Some l -> l
                                      | _ -> [])
            |> List.filter (fun img -> img.typ.StartsWith "image/")
        }

    let getImgPath img =
        let substChars = function
                       | '/' -> '_'
                       | c -> c

        let filename = img.url
                       |> Seq.map substChars
                       |> System.String.Concat

        sprintf "/tmp/ImgurSlideshow/images/%A" filename

    let downloadBinary url path =
        async {
          let! req = Http.AsyncRequest (url, headers = [ "Authorization", auth ])
          match req.Body with
          | Binary bytes ->
              System.IO.File.WriteAllBytes(path, bytes)
              return Some path
          | Text _ -> return None
        }

    let prepareStorage () =
        System.IO.Directory.CreateDirectory "/tmp/ImgurSlideshow/images" |> ignore

    let getImage img =
        async {
          let imgPath = getImgPath img
          if System.IO.File.Exists imgPath
          then
              return Some imgPath
          else
              return! downloadBinary img.url imgPath
        }

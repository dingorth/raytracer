open Raymod;;

let () =
    let argNr = Array.length Sys.argv - 1 in
    if argNr < 2
    then
        Printf.fprintf stdout "usage: ./main.native [json_file] [save/show] [save_img_path opt] \n"
    else
        let json_file = Sys.argv.(1) in
        let option = Sys.argv.(2) in
        match option with
            | "show" ->  
                let json = Yojson.Basic.from_file json_file in
                let open Yojson.Basic.Util in
                let camera = json |> member "camera" |> parse_camera in
                let scene = json |> member "scene" |> parse_scene in
                let picture = raytrace camera scene in
                draw_picture picture camera; 
                let  _ = read_line () in ()
            | "save" -> if argNr == 3 then
                let json = Yojson.Basic.from_file json_file in
                let open Yojson.Basic.Util in
                let camera = json |> member "camera" |> parse_camera in
                let scene = json |> member "scene" |> parse_scene in
                let picture = raytrace camera scene in
                save_picture Sys.argv.(3) picture camera
                else Printf.fprintf stdout "usage: ./main.native [json_file] [save/show] [save_img_path opt] \n"
            | _ -> Printf.fprintf stdout "usage: ./main.native [json_file] [save/show] [save_img_path opt] \n"

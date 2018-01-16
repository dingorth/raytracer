open Core.Std;;
open Vector3d;; 
open Graphics;;
module J = Yojson;;
module V = Vector3d.Vec3d;;

type pixels = V.t list
(* generate_picture : picture -> pixels *)

let generate_picture p = []


let () =
    let argNr = Array.length Sys.argv - 1 in
    if argNr <> 2
    then
        Printf.fprintf stdout "wrong parameters\n"
    else
        let json_file = Sys.argv.(1) in
        let json = Yojson.Basic.from_file json_file in
        
        let open Yojson.Basic.Util in
        let scene_json = json |> member "scene" in
        let camera_json = json |> member "camera" in
        print_string "parsed"        


;;
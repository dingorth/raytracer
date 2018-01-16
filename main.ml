open Core.Std;;
open Vector3d;; 
open Graphics;;
module J = Yojson;;
module V = Vector3d.Vec3d;;

type pixels = V.t list
(* generate_picture : picture -> pixels *)

let generate_picture p = []

let parse_camera camera_json =
    let open Yojson.Basic.Util in
    let position_json = camera_json |> member "position" in
    let focus = camera_json |> member "focus" |> to_list |> filter_float |> V.create_from_list in
    let resolution_json = camera_json |> member "resolution" in

    let position_a_vect = position_json |> member "a" |> to_list |> filter_float |> V.create_from_list in
    let position_b_vect = position_json |> member "b" |> to_list |> filter_float |> V.create_from_list in
    let position_c_vect = position_json |> member "c" |> to_list |> filter_float |> V.create_from_list in
    let position_d_vect = position_json |> member "d" |> to_list |> filter_float |> V.create_from_list in

    let camera_position = position_a_vect, position_b_vect, position_c_vect, position_d_vect in

    let resolution = 
        let foo = function 
            | [x; y] -> x, y
            | _ -> failwith "wrong resolution"        
        in
        resolution_json |> to_list |> filter_int |> foo
    in

    let pixel_width = 1.0 in (* policzyć to !!!!!!!!!! *)
    
    camera_position, focus, pixel_width, resolution

(* let parse_object robject_json = *)

(* let pasrse_scene scene_json *)

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
        print_string "parsed\n"
;;
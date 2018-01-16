open Core.Std;;
open Vector3d;; 
open Graphics;;
module J = Yojson;;
module V = Vector3d.Vec3d;;
open Raytypes;;

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

let parse_sphere shape_json = 
    let open Yojson.Basic.Util in
    let center = shape_json |> member "center" |> to_list |> filter_float |> V.create_from_list in
    let radius = shape_json |> member "radius" |> to_float in
    ((new sphere center radius) :> shape)

let parse_plane shape_json = 
    let open Yojson.Basic.Util in
    let abcd = shape_json |> member "parameters" |> to_list |> filter_float in
    let v, d = match abcd with 
        | [a; b; c; d] -> V.create a b c, d
        | _ -> failwith "wrong plane parameters" in 
    ((new plane v d) :> shape)

let parse_shape shape_json =
    let open Yojson.Basic.Util in
    let shape_type = shape_json |> member "type" |> to_string in
        match shape_type with
            | "sphere" -> parse_sphere shape_json
            | "plane" -> parse_plane shape_json
            | _ -> failwith "wrong shape type"

let parse_scatter surface_json =
    let open Yojson.Basic.Util in
    let color = surface_json |> member "color" |> to_list |> filter_float |> V.create_from_list in
    ((new scatter color) :> ((light robject, light) scatter)) 

let parse_surface surface_json =
    let open Yojson.Basic.Util in
    let surface_type = surface_json |> member "type" |> to_string in
        match surface_type with
            | "scatter" -> parse_scatter surface_json
            | _ -> failwith "wrong surface type"

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



type pixels = V.t list

(* generate_picture : picture -> pixels *)
let generate_picture p = []
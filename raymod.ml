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

    let pixel_width = (V.dist position_a_vect position_b_vect) /. float_of_int (fst resolution) in
    let pixel_height = (V.dist position_a_vect position_c_vect) /. float_of_int (snd resolution) in
    
    camera_position, focus, pixel_width, pixel_height, resolution

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

let parse_light_central light_json = 
    let open Yojson.Basic.Util in
    let position = light_json |> member "position" |> to_list |> filter_float |> V.create_from_list in
    let color = light_json |> member "color" |> to_list |> filter_float |> V.create_from_list in
    ((new central position color) :> light)

let parse_light_sunlight light_json = 
    let open Yojson.Basic.Util in
    let direction = light_json |> member "direction" |> to_list |> filter_float |> V.create_from_list in
    let color = light_json |> member "color" |> to_list |> filter_float |> V.create_from_list in
    ((new sunlight direction color) :> light)

let parse_light light_json = 
    let open Yojson.Basic.Util in
    let light_type = light_json |> member "type" |> to_string in
        match light_type with
            | "central" -> parse_light_central light_json
            | "sunlight" -> parse_light_sunlight light_json
            | _ -> failwith "wrong light type"

let parse_scatter surface_json =
    let open Yojson.Basic.Util in
    let color = surface_json |> member "color" |> to_list |> filter_float |> V.create_from_list in
    ((new scatter color) :> surface')

let parse_surface surface_json =
    let open Yojson.Basic.Util in
    let surface_type' = surface_json |> member "type" |> to_string in
        match surface_type' with
            | "scatter" -> parse_scatter surface_json
            | _ -> failwith "wrong surface type"

let parse_robject robject_json =
    let open Yojson.Basic.Util in
    let surface = robject_json |> member "surface" |> parse_surface in
    let shape = robject_json |> member "shape" |> parse_shape in
    (new robject (shape :> shape) (surface :> surface'))

let parse_scene scene_json = 
    let open Yojson.Basic.Util in
    let lights = List.map parse_light (scene_json |> member "lights" |> to_list)  in
    let robjects = List.map parse_robject (scene_json |> member "robjects" |> to_list) in
    Scene(robjects, lights)

(* DONE PARSING *)

let smallest_positive_distance ray robj =
    let t_list = robj#shape#intersect ray in
    try 
        let t = t_list |> List.sort compare |> List.find ((<) 0.) in
        One(t) (* multiply param t times ray direction vector length *)
    with
        Not_found -> None

let closest_object ray objects = 
    let rec foo = function
        | [] -> NoneI
        | o::os -> let rest = foo os and o_smallest = smallest_positive_distance ray o in match rest, o_smallest with
            | NoneI, None -> NoneI
            | NoneI, One(t) -> OneI(o,t)
            | OneI(o',t'), None -> OneI(o',t')
            | OneI(o',t'), One(t) -> if t' < t then OneI(o',t') else OneI(o,t)
    in
    foo objects

let print_color = function (r,g,b) -> 
    print_float r; print_float g; print_float b

let cast_ray source destination scene =
    let ray = Ray(source, destination) in
    let objects = get_scene_objects scene in
    let lights = get_scene_lights scene in
    let obj = closest_object ray objects in
    match obj with
        | NoneI -> V.create 0. 0. 0.
        | OneI(o,t) -> let shape = o#shape and surface = o#surface in
            let isect_point = V.mul_scalar t (ray_dir ray) in
            let normal_v = shape#normal isect_point in
            let tmp = surface#color ray isect_point normal_v objects lights in
            tmp

(* column x row *)
let generate_picture_pixel_grid width height =
    let rec bar param max acc iter = match iter with
        | 0 -> acc 
        | n -> bar param max ((max - n, param)::acc) (n-1)
    in
    let rec foo acc iter = match iter with
        | 0 -> acc
        | n -> foo ((bar (height-n) width [] width) @ acc) (n-1)
    in
    foo [] height;;

let generate_picture_points camera =
    let width_int, height_int = get_camera_resolution camera in 
    let pixel_height = get_camera_pixel_height camera in
    let pixel_width = get_camera_pixel_width camera in
    let a_point = get_camera_position camera |> get_position_a in
    let b_point = get_camera_position camera |> get_position_b in
    let c_point = get_camera_position camera |> get_position_c in
    let a_to_b_vec = V.sub b_point a_point |> V.normalize |> V.mul_scalar pixel_width in
    let a_to_c_vec = V.sub c_point a_point |> V.normalize |> V.mul_scalar pixel_height in
    let p  = generate_picture_pixel_grid width_int height_int in
    let rtn = List.map (fun e -> e, a_point |> V.add (V.mul_scalar (float_of_int @@ fst e) a_to_b_vec) |> V.add (V.mul_scalar ( float_of_int @@ snd e) a_to_c_vec) ) p in
    rtn

let raytrace camera scene = 
    let pixels = generate_picture_points camera in
    List.map (fun p -> 
        match p with (pixeli, pixelf) -> 
            let clr = cast_ray pixelf (get_camera_focus camera) scene in
            Pixel(pixeli, pixelf, clr)
        ) pixels

let draw_picture picture camera = 
    let res = get_camera_resolution camera in
    let width_int = fst res and height_int = snd res in
    Graphics.open_graph (" " ^ string_of_int width_int ^ "x" ^ string_of_int height_int);
    List.iter (fun pixel -> 
        let pixel_color_vect = get_pixel_color pixel in 
        let trimmed_r, trimmed_g, trimmed_b = pixel_color_to_int_with_trim pixel_color_vect in
        let graphics_color = Graphics.rgb trimmed_r trimmed_g trimmed_b in
        Graphics.set_color graphics_color; 
        Graphics.plot (width_int - (get_pixel_i pixel |> fst)) (height_int - (get_pixel_i pixel |> snd))
        ) picture;
    ()

let save_picture path picture camera =
    let channel = open_out path in
    let res = get_camera_resolution camera in
    let width_int = fst res and height_int = snd res in
    Printf.fprintf channel "P3 \n%d %d\n 255\n" width_int height_int;
    List.iter (fun pixel ->
        let pixel_color_vect = get_pixel_color pixel in
        let trimmed_r, trimmed_g, trimmed_b = pixel_color_to_int_with_trim pixel_color_vect in
        Printf.fprintf channel "%d %d %d\n" trimmed_r trimmed_g trimmed_b
    ) picture 
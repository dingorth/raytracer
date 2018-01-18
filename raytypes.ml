module V = Vector3d.Vec3d

type ray = Ray of V.t * V.t
type normal_vect = V.t
type intersect_point = V.t
let ray_src = function Ray(s,_) -> s
let ray_dest = function Ray(_,d) -> d
let ray_dir = function Ray(s,d) -> V.sub d s
let ray_print = function Ray(s,d) ->
    print_string "Ray("; V.print s; V.print d; print_char ')'; print_char '\n'

type color = V.t

let solve_quadratic a b c = 
    let delta = b*.b -. 4.*.a*.c in
    if delta < 0. then []
    else 
        if delta == 0. then [-.b /. (2.*.a)]
        else [(-.b +. sqrt delta) /. (2. *. a); (-.b -. sqrt delta) /. (2. *. a)]

(* SHAPES *)

class virtual shape = object
    method virtual intersect: ray -> float list
    method virtual normal: V.t -> V.t
end

let print_float_list l =
    print_string "d_list: "; List.iter  print_float l; print_string "\n"

class sphere c r = object
inherit shape

    val center: V.t = c
    method center = center

    val radius: float = r
    method radius = radius

    method intersect r = 
        let lsq v = V.dot v v in
        let a = lsq (ray_dir r) and
        b = 2. *. ((V.dot (ray_src r) (ray_dir r)) -. ( V.dot center (ray_dir r)  )  ) and
        c = lsq (ray_src r) +. lsq center -. 2. *. (V.dot (ray_src r) center) -. radius*.radius in
        let solved = solve_quadratic a b c in
        print_float_list solved;
        solved

    method normal v = V.normalize (V.sub v center)
end

class plane v d = object
inherit shape
    
    val abc = v
    val d = d
    
    method intersect r = 
        let denom = V.dot abc (ray_dir r) in
            (* ray_print r; *)
            if denom == 0. then []
            else
                let t = (-.(V.dot abc (ray_src r)) -. d ) /. denom in
                (* print_float t; *)
                [t]
           
    method normal _ = V.normalize abc
end

(* SURFACES *)

class virtual ['a, 'b] surface = object
    (* 'a == light robject, 'b == light *)
    method virtual color : ray -> intersect_point -> normal_vect -> 'a list -> 'b list -> color
end

class ['a, 'b] scatter (color': color) = object
inherit ['a, 'b] surface
(* 'a == light robject, 'b == light *)

    val color'' = V.normalize color'

    method color (ray:ray) (isect_point:intersect_point) (normal_vect:normal_vect) (lrobject: 'a list) (llight: 'b list) = 
        let light_unreachable (l : 'b) = 
            let isect_to_light = Ray(isect_point, V.negate (l#direction isect_point)) in (* uwaga na kierunek swiatla *)
            List.exists (fun o -> 
                let d_list = o#shape#intersect isect_to_light in 
                    List.exists (fun e -> e < 1. && e > 0.) d_list
                ) 
                lrobject
        in
        let rec light_iter acc = function
            | [] -> acc
            | l::ls -> 
                if (light_unreachable l) == false 
                then
                    let isect_to_light_vect = (V.negate (l#direction isect_point)) in
                    (* let _ = V.print isect_to_light_vect; print_char '\n' in *)
                    let normal' = if V.dot isect_to_light_vect normal_vect < 0. then    (* chce zeby normalny byl po tej stronie plaszczyzny co wektor do swiatla *)
                        V.negate normal_vect else normal_vect in
                    if V.dot isect_to_light_vect normal' < 0. 
                    then light_iter acc ls
                    else 
                        let angle = (V.cos normal' isect_to_light_vect) in (* cos czy dot?? *)
                        let angle_multiplied = (V.mul_scalar angle (l#intensity isect_point)) in
                        let current_ligth_color = V.mul angle_multiplied color'' in
                        let new_acc = V.add acc current_ligth_color in 
                            light_iter new_acc ls 
                else light_iter acc ls
        in
        light_iter (V.create 0. 0. 0.) llight
end

(* OBJECT *)

class ['b] robject (sh : shape) (surf : (('self, 'b) surface)) = object(self : 'self)
(* 'b == light *)
    val shape = sh
    method shape = shape

    val surface = surf
    method surface = surface
end

(* LIGHT *)

class virtual light = object
    method virtual intensity : intersect_point -> color
    method virtual direction : intersect_point -> V.t

end

class central (pos : V.t) (pwr : color) = object
    val position = pos
    val power = pwr

    method intensity i =
        let d = V.length @@ V.sub i pos in
        V.div_scalar (d*.d) pwr
    
    method direction i = V.sub i position
end

(* SCENE *)
type scene = Scene of light robject list * light list
let get_scene_objects = function Scene(o, _) -> o
let get_scene_lights = function Scene(_, l) -> l

(* CAMERA *)
type resolution = int * int
type pixel_width = float   (* width of int *)
type pixel_height = float (* height of int *)
type focus = V.t
type camera_position = V.t * V.t * V.t * V.t
let get_position_a = function (a,_,_,_) -> a
let get_position_b = function (_,b,_,_) -> b
let get_position_c = function (_,_,c,_) -> c
let get_position_d = function (_,_,_,d) -> d
type camera = camera_position * focus * pixel_width * pixel_height * resolution
let get_camera_position = function (p,_,_,_,_) -> p
let get_camera_focus = function (_,f,_,_,_) -> f
let get_camera_pixel_width = function (_,_,pw,_,_) -> pw
let get_camera_pixel_height = function (_,_,_,ph,_) -> ph
let get_camera_resolution (c:camera) : resolution = match c with (_,_,_,_,r) -> r

(* concrete types aliases *)
type surface' = (light robject, light) surface
type robject' = light robject

(* picture pixels *)
type pixel_f = V.t
type pixel_i = int * int
type pixel = Pixel of pixel_i * pixel_f * color
let get_pixel_f = function Pixel(_,f,_) -> f
let get_pixel_i = function Pixel(i,_,_) -> i
let get_pixel_color = function Pixel(_,_,c) -> c

let pixel_color_to_int_with_trim = function (r,g,b) -> 
    let foo f = if f > 255. then 255. else f in
    (int_of_float @@ foo r, int_of_float @@ foo g, int_of_float @@ foo b)


type picture = pixel list

(* ... *)
type distance = float
type intersecting_obj = NoneI | OneI of robject' * distance
type ray_obj_dist = None | One of float
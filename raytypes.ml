module V = Vector3d.Vec3d

type ray = Ray of V.t * V.t
type normal_vect = V.t
type intersect_point = V.t
let ray_src = function Ray(s,_) -> s
let ray_dir = function Ray(_,d) -> d

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
        solve_quadratic a b c

    method normal v = V.normalize (V.sub v center)
end

class plane v d = object
inherit shape
    
    val abc = v
    val d = d
    
    method intersect r = 
        let denom = V.dot abc (ray_dir r) in
            if denom == 0. then []
            else [(-.(V.dot abc (ray_src r)) -. d ) /. denom]
           
    method normal _ = V.normalize abc
end

(* SURFACES *)
class virtual ['a, 'b] surface = object
    (* 'a == robject, 'b == light *)
    method virtual color : ray -> intersect_point -> normal_vect -> 'a list -> 'b list -> color
end

class ['a, 'b] scatter color' = object
inherit ['a, 'b] surface

    val color'' = color'

    method color ray isect_point normal_vect lrobject llight = 
        let light_unreachable l = 
            let isect_to_light = Ray(isect_point, V.negate (l#direction isect_point)) in (* uwaga na kierunek swiatla *)
            List.exists (fun o -> 
                let d_list = o#shape#intersect isect_to_light in 
                    List.exists (fun e -> e < 1 && e > 0) d_list
                ) 
                lrobject
        in
        let rec light_iter acc = function
            | [] -> V.create 0. 0. 0.
            | l::ls -> 
                if (light_unreachable l) == false 
                then
                    let isect_to_light_vect = (V.negate (l#direction isect_point)) in
                    let normal' = if V.dot isect_to_light_vect normal_vect < 0. then
                        V.negate normal_vect else normal_vect in
                    if V.dot isect_to_light_vect normal' < 0. 
                    then light_iter acc ls
                    else light_iter (V.add acc ( V.mul (V.mul_scalar (V.dot normal' isect_to_light_vect) (l#intensity isect_point)) color'' )) ls 
                else light_iter acc ls
        in
        light_iter (V.create 0. 0. 0.) llight
end

(* OBJECT *)
class ['a] robject (sh : shape) (surf : (('self, 'a) surface)) = object(self : 'self)
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
        let d = V.length @@ V.sub pos i in
        V.div_scalar (d*.d) pwr
    
    method direction i = V.sub position i
    
end

(* SCENE *)
type scene = Scene of light robject list * light list

(* CAMERA *)
type resolution = int * int
type pixel_width = float   (* width of int *)
type focus = V.t
type camera_position = V.t * V.t * V.t * V.t

type camera = camera_position * focus * pixel_width * resolution

type picture = scene * pixel_width

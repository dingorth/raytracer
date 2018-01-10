open Core.Std
open Vector3d;;
module V = Vector3d.Vec3d

type ray = Ray of V.t * V.t
let ray_src = function Ray(s,_) -> s
let ray_dst = function Ray(_,d) -> d

class virtual shape = object(self)
    method virtual intersect: ray -> float list 
    method virtual normal: V.t -> V.t
end

class sphere x y z r = object
inherit shape

    val center: V.t = V.create x y z
    method center = center

    val radius: float = r
    method radius = radius

    method intersect r = []

    method normal v = V.normalize (V.sub v center)

end

class plane a b c d = object
inherit shape
    
    val abc = V.create a b c
    val d = d
    
    method intersect r = []
           
    method normal _ = V.normalize abc
end
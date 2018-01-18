module Vec3d =
struct
    type t = float * float * float
    type scalar = float
    
    let create x y z = x, y, z
    let create_from_list (l : float list) : t = match l with 
        | [x; y; z] -> x, y, z 
        | _ -> failwith "not 3d vector"

    let map f = function (x,y,z) -> (f x, f y, f z)
    let zip_with f v1 v2 = match v1, v2 with 
        (x1,y1,z1), (x2,y2,z2) -> (f x1 x2, f y1 y2, f z1 z2)
    let fold f s = function (x,y,z) -> f (f (f s x) y) z

    let negate v = map (~-.) v
    let add v1 v2 = zip_with (+.) v1 v2
    let sub v1 v2 = zip_with (-.) v1 v2
    let mul v1 v2 = zip_with ( *.) v1 v2
    let mul_scalar s v = map (fun x -> x *. s) v
    let div_scalar s v = map (fun x -> x /. s) v

    let dot v1 v2 = zip_with ( *.) v1 v2 |> fold (+.) 0.
    let length v = dot v v |> sqrt
    let normalize v = div_scalar (abs_float (length v)) v

    let dist v1 v2 = let l = sub v1 v2 in dot l l |> sqrt

    let print = function (x,y,z) -> 
        print_char '[';
        print_float x; print_char ' '; 
        print_float y; print_char ' ';
        print_float z; print_char ']'

    let cos v1 v2 = dot v1 v2 /. ((length v1) *. (length v2))
end
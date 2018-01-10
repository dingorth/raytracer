open Core.Std;;
open Vector3d;; (* Vec3d *)
open Raytypes
module J = Yojson;;
module V = Vector3d.Vec3d


open Graphics;;

print_string "Hello world!\n";;

let o1 = new sphere 1. 2. 3. 4.
let o2 = new plane 1. 4. 5. 2.

let list = [(o1 :> shape); (o2 :> shape)]



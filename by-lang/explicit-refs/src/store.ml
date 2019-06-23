type t = Val.t list ref

let v = (ref [] : t)
let n = ref 0

let initialize () = v := []

let newref x =
  begin
    incr n;
    v := x :: !v;
    !n
  end

let deref r = List.nth !v (!n - r)

let setref r x =
  let rec f m = function
    | [] -> failwith "Reference does not exist"
    | y::ys ->
        if m = 0 then x :: ys
        else y :: (f (m - 1) ys)
  in
  (v := f (!n - r) !v; x)

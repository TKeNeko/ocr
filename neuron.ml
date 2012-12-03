class neuron matrix character =
object (self)
  val mat : int array array = matrix
  val letter : char = character
  val size_x : int = Array.length matrix
  val size_y : int = Array.length matrix.(0)
  
  method get_letter = letter
  method compare matrix =
    let c = ref 0 in 
    for x = 0 to (size_x - 1) do
      for y = 0 to (size_y - 1) do
	begin
	if (matrix.(x).(y) = mat.(x).(y)) then
	  incr c;
	end
      done;
    done;
    !c

  method matching matrix =
    let c = self#compare matrix in
    let use = size_x * size_y / 2 in
    c >= use * 7 / 10    
end

let get_dims matrix = (Array.length matrix, Array.length matrix.(0))

let detect_top matrix = 
  let (x,y) = get_dims matrix
  and stop = ref false
  and i = ref 0 
  and j = ref 0 in
  while !i < x && not !stop do
    j := 0;
    while !j < y && not !stop do
      stop := (matrix.(!i).(!j) <> 0);
      j:= !j+1
    done;
    i := !i + 1
  done;
  (!i-1)

let detect_left matrix =
  let (x,y) = get_dims matrix in
  let i = ref 0 in
  let j = ref 0 in
  let stop = ref false in
  while !j < y && not !stop do
    i := 0;
    while !i < x && not !stop do
      stop := (matrix.(!i).(!j) <> 0);
      i := !i + 1;
    done;
    j := !j + 1
  done;
  !j - 1;;

let detect_down matrix =
  let (x,y) = get_dims matrix in
  let i = ref (x - 1) in
  let j = ref 0 in
  let stop = ref false in
  while !i >= 0 && not !stop do
    j := 0;
    while !j < y && not !stop do
      stop := (matrix.(!i).(!j) <> 0);
      j := !j + 1;
    done;
    i := !i - 1;
  done;
  !i + 1;;

let detect_right matrix =
  let (x,y) = get_dims matrix in
  let stop = ref false
  and i = ref 0
  and j = ref (y-1) in
  while !j > 0  && not !stop do
    i := 0;
    while !i < x && not !stop do
      stop := (matrix.(!i).(!j) <> 0);
      i := !i+1
    done;
    j := !j - 1;
  done;
  (!j + 1)

let extend_matrix matrix dest_x dest_y =
  let (x,y) = get_dims matrix in
    if (dest_x <= x) || (dest_y <= y) then
      failwith "Dimensions plus petites que la matrice d'origine"
    else
      let dest_mat = Array.make_matrix dest_x dest_y 0 in
      for i = 0 to x - 1 do
	for j = 0 to y - 1 do
	  begin
	    if (matrix.(i).(j) <> 0) then
	       
	  end
	done
      done

let test =
  let mat = Array.make_matrix 2 3 0 in
  mat.(1).(2) <- 1;
  extend_matrix mat 3 4

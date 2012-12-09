let size_mat_x = 8
let size_mat_y = 8
	
let get_dims matrix = (Array.length matrix, Array.length matrix.(0))

let print_mat matrix =
  let (x,y) = get_dims matrix in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      print_string (string_of_int(matrix.(i).(j)))
    done;
    print_string "\r\n"
  done

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

let float_to_int f =
  if (f -. float_of_int(int_of_float f) >= 0.5) then
    int_of_float f + 1
  else
    int_of_float f

let extend_mat_w matrix dest_y =
  let (x,y) = get_dims matrix in
  let dest_mat = Array.make_matrix x dest_y 0 in
  let mult_y = float_to_int((float dest_y) /. (float y)) in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      if (matrix.(i).(j) <> 0) then
	for m = 0 to mult_y - 1 do
	  let pos_y = mult_y * j + m in
	  if (pos_y < dest_y) then
	    dest_mat.(i).(pos_y) <- 1;
	done
    done
  done;
  dest_mat

let extend_mat_h matrix dest_x =
  let (x,y) = get_dims matrix in
  let dest_mat = Array.make_matrix dest_x y 0 in
  let mult_x = float_to_int((float dest_x) /. (float x)) in
  for i = 0 to x - 1 do
    for j = 0 to y - 1 do
      if (matrix.(i).(j) <> 0) then
	for n = 0 to mult_x - 1 do
	  let pos_x = mult_x * i + n in
	  if (pos_x < dest_x) then
	    dest_mat.(pos_x).(j) <- 1;
	done
    done
  done;
  dest_mat

let reduce_mat_w matrix dest_y = 
  let (x,y) = get_dims matrix in
  let mult_y = float_to_int((float y) /. (float dest_y)) in
  let dest_mat = Array.make_matrix x dest_y 0
  and col = ref 0
  and max_col = ref 0
  and sum = ref 0 in
  for i = 0 to x - 1 do
    col := 0;
    max_col := 0;
    for j = 0 to dest_y - 1 do
      begin
	sum := 0;
	max_col := !max_col + mult_y; 
	while(!col < y && !col < !max_col) do
	  sum := !sum + matrix.(i).(!col);
	  col:= !col + 1
	done;
	if (float !sum) > ((float mult_y) /. 1.8) then
	  dest_mat.(i).(j) <- 1
      end
    done
  done;
  dest_mat

let reduce_mat_h matrix dest_x =
  let (x,y) = get_dims matrix in
  let mult_x = float_to_int((float x) /. (float dest_x)) in
  let dest_mat = Array.make_matrix dest_x y 0
  and line = ref 0
  and max_line = ref 0
  and sum = ref 0 in
  for j = 0 to y - 1 do
    line := 0;
    max_line := 0;
    for i = 0 to dest_x - 1 do
      begin
	sum := 0;
	max_line := !max_line + mult_x; 
	while(!line < x && !line < !max_line) do
	    sum := !sum + matrix.(!line).(j);
	    line := !line + 1
	done;
	if (float !sum) > ((float mult_x) /. 1.8) then
	    dest_mat.(i).(j) <- 1
      end
    done
  done;
  dest_mat

let truncate matrix = 
  let border_top = detect_top matrix
  and border_right = detect_right matrix
  and border_down = detect_down matrix
  and border_left = detect_left matrix in
  let col = (border_right - border_left) + 1
  and lines = (border_down - border_top) + 1 in
  let trunc_mat = Array.make_matrix lines col 0
  and x = ref 0
  and y = ref 0 in
  for i = border_top to border_down do
    y := 0;
    for j = border_left to border_right do
      trunc_mat.(!x).(!y) <- matrix.(i).(j);
      y := !y + 1
    done;
    x := !x +1
  done;
  trunc_mat

let resize matrix =
  let (x,y) = get_dims matrix
  and dest_mat = ref matrix in
  begin
    if (x > size_mat_x) then
      dest_mat := reduce_mat_h matrix size_mat_x
    else if (x < size_mat_x) then
      dest_mat := extend_mat_h matrix size_mat_x
  end;
  begin
    if (y > size_mat_y) then
      dest_mat := reduce_mat_w !dest_mat size_mat_y
    else if (y < size_mat_y) then
      dest_mat := extend_mat_w !dest_mat size_mat_y
  end;
  !dest_mat

class neuron character =
object (self)
  val mutable letter : char = character
  val size_x : int = size_mat_x
  val size_y : int = size_mat_y
  val mutable matrix_weight : float array array = Array.make_matrix size_mat_x size_mat_y 0.

  method get_letter = letter
  method get_matrix_weight = matrix_weight

  method learning tab_mat character =
    let number = Array.length tab_mat
    and mat_temp = ref (Array.make_matrix size_x size_y 0) in
    for i = 0 to (number - 1) do
      mat_temp := tab_mat.(i);
      for x = 0 to (size_x - 1) do
	for y = 0 to (size_y - 1) do
	    matrix_weight.(x).(y) <- matrix_weight.(x).(y) +. (float !mat_temp.(x).(y))
	done;
      done;
    done;
    for x = 0 to (size_x - 1) do
      for y = 0 to (size_y - 1) do
	matrix_weight.(x).(y) <- matrix_weight.(x).(y) /. (float number)
      done;
    done;
    letter <- character;

    method matching matrix =
      let sum = ref 0. in
      for x = 0 to (size_x - 1) do
	for y = 0  to (size_y - 1) do
	  if matrix.(x).(y) = 1 then
	    sum := !sum +. matrix_weight.(x).(y)
	  else
	    sum := !sum -. matrix_weight.(x).(y)
	done
      done;
      !sum

    method get_weight i j =
      string_of_float(matrix_weight.(i).(j))
end

let is_empty mat = 
  let (x,y) = get_dims mat 
  and sum = ref 0
  and i = ref 0
  and j = ref 0 in
  while !i < x do
    j := 0;
    while !j < y do
      sum := !sum + mat.(!i).(!j);
      j := !j + 1;
    done;
    i := !i + 1
  done;
  (!sum = 0)

class network nbr = 
object (self)
  val tab = Array.make nbr (new neuron 'a')

  method initialize = 
    for i = 0 to (nbr - 1) do
      tab.(i) <- new neuron (Char.chr (i + 33))
    done
      
  method make_clean matrix = resize (truncate matrix)

  method learning_net tab_mat tab_char= 
    for i = 0 to (nbr - 1) do
      tab.(i)#learning tab_mat.(i) tab_char.(i)
    done
      
  method recongnize matrix =
    let high = ref (-64.)
    and level = ref 0.
    and num = ref 0
    and perf_matrix = resize (truncate matrix) in
    for i = 0 to (nbr - 1) do
      level := (tab.(i)#matching perf_matrix);
      if !level >= !high then
	begin 
	  high := !level;
	  num := i
	end
    done;
    tab.(!num)#get_letter

  method mat_to_string mat =
    if is_empty mat then
      " "
    else
      let char_to_string = function
	|'a' -> "a"
	|_ -> ""
      in
      char_to_string (self#recongnize mat)

  method save =
    let save_neuron = open_out "save_neuron.txt" in
    let ligne = ref "" in
    for n = 0 to nbr - 1 do
      let mat = tab.(n)#get_matrix_weight in
      for i = 0 to size_mat_x - 1 do
	for j = 0 to size_mat_y - 1 do
	  ligne := !ligne ^ (string_of_float(mat.(i).(j))) ^ " "
	done
      done;
      output_string save_neuron (Printf.sprintf "%s" (!ligne ^ "\n"))
    done;
    close_out save_neuron
end

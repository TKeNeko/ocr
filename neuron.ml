class neuron

end

let truncate matrix = 
  let (x,y) = (Array.length matrix ,Array.length matrix.(0))
  and sum = ref 0
  and i = ref 0 
  and j = ref 0 in
  let rec trunc mat bi_x bs_x bi_y bs_y =
    i := bi_x;
    sum := 0;
    while !i < bs_x && !sum = 0 do
      j := bi_y;
      while !j < bs_y do
	sum := !sum + mat.(!i).(!j);
	j:= !j+1
      done;
      i := !i + 1
    done;
    (!i-1)
  in trunc matrix 0 y 0 x


let test =
  let mat = Array.make_matrix 2 3 0 in
  mat.(1).(2) <- 1;
  truncate mat

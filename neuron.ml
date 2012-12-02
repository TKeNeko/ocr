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

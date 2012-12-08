(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
    
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
    Sdlevent.KEYDOWN _ -> ()
  | _ -> wait_key ()
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst

(* level *)
let level (r,g,b) = (0.3 *. (float  r) +. 0.59 *. (float g) +. 0.11 *. (float b)) /. 255.

(* color2grey *)
let color2grey (r,g,b) = 
  let grey = int_of_float (level (r,g,b) *. 255.)
  in (grey,grey,grey)

(* grey2black_white 
   step comprit 0 et 1 *)
let grey2black_white src step = 
  let (x,y) = get_dims src 
  in let min = int_of_float(255. *. step)
     in for i = 0 to (x-1) do
         for j = 0 to (y-1) do
	   let (r,g,b) = Sdlvideo.get_pixel_color src i j in
	   if r >= min then
	     Sdlvideo.put_pixel_color src i j (255,255,255)
	   else
	     Sdlvideo.put_pixel_color src i j (0,0,0)
	 done
       done		 

(* image2grey *)
let image2grey src dst = 
  let (x,y) = get_dims src
  in for i = 0 to (x-1) do
      for j = 0 to (y-1) do
	let color = Sdlvideo.get_pixel_color src i j in 
	Sdlvideo.put_pixel_color dst i j (color2grey color)
      done
    done

let rec tri tab i =
  if i > 0 then
    tri (tab) (i-1); let k = ref (i-1) in 
		     let x = tab.(i) in
		     while !k > 0 && tab.(!k) > x do
		       tab.(!k+1) <- tab.(!k);
		       k := !k - 1
		     done; 
		     tab.(!k+1) <- x
		       
let around3x3 i j src =
  let tab = Array.make 9 (0,0,0) in 
  for k = 0 to 2 do
    tab.(k) <- Sdlvideo.get_pixel_color src (i - 1) (j + k - 1);
    tab.(k+3) <- Sdlvideo.get_pixel_color src i (j + k - 1);
    tab.(k+6) <- Sdlvideo.get_pixel_color src (i + 1) (j + k - 1)
  done; tab
    
    
let remove_noise src = 
  let (x,y) = get_dims src and tab = ref (Array.make 9 (0,0,0))
  in for i = 1 to (x-2) do
      for j = 1 to (y-2) do
	tab := around3x3 i j src; 
	tri !tab 8;
	if Sdlvideo.get_pixel_color src i j <> !tab.(4) then
	  Sdlvideo.put_pixel_color src i j !tab.(4)
      done
    done

let filter src mat pds = 
  let (x,y) = get_dims src and tab = ref (Array.make 9 (0,0,0)) in
  let sum = ref 0 in
  for i = 1 to (x-2) do
    for j = 1 to (y-2) do
      tab := around3x3 i j src;
      for k = 0 to 2 do
	let (r,g,b) = !tab.(k) in
	sum := r * mat.(0).(k mod 3) + !sum;
	let (r1,g1,b1) = !tab.(k+3) in
	sum := r1 * mat.(1).((k mod 3)) + !sum;
	let (r2,g2,b2) = !tab.(k+6) in
	sum := r2 * mat.(2).((k mod 3)) + !sum
      done;
      sum := !sum / pds;
      Sdlvideo.put_pixel_color src i j (!sum,!sum,!sum);
      sum := 0;
    done 
  done 

let convolution src = 
  let mat = Array.make_matrix 3 3 1 in
  mat.(0).(1) <- 2;
  mat.(1).(0) <- 2;
  mat.(1).(1) <- 5;
  mat.(1).(2) <- 2;
  mat.(2).(1) <- 2;
  filter src mat 17

let pi = 3.141592654;;

let rot_img img dst ang =
  let (w,h) = get_dims img in
  let (center_x, center_y) = (w / 2, h / 2) in
  let ang_r = (ang *. pi) /. 180. in
  for y = 0 to (h-1) do
    for x = 0 to (w-1) do
      let x2 = (float x) -. (float center_x) in
      let y2 = (float y) -. (float center_y) in
      let old_x = x2 *. cos(ang_r) -. y2 *. sin(ang_r) in
      let int_old_x = int_of_float old_x + center_x in
      let old_y = x2 *. sin(ang_r) +. y2 *. cos(ang_r) in
      let int_old_y = int_of_float old_y + center_y in
      if (int_old_x < 0 || int_old_x > w ||
            int_old_y < 0 || int_old_y > h) then
        Sdlvideo.put_pixel_color dst x y (255, 255, 255)
      else
        let color = Sdlvideo.get_pixel_color img int_old_x int_old_y in
        Sdlvideo.put_pixel_color dst x y color
    done
  done

let max_mat mat = 
  let dims mat =
    let n = Array.length mat in
    if n = 0 then (0, 0) else (n, Array.length mat.(0)) in
  let (i, j) = dims mat in
  let p_max = ref 0 in
  let theta = ref 0 in
  for x = 0 to i-1 do
    for y = 0 to j-1 do
      if mat.(x).(y) > !p_max then
	begin
	  p_max := mat.(x).(y);
	  theta := y;
	end
    done;
  done;
  !theta;;

let detect_rot img =
  let (w, h) = get_dims img in
  let lines = int_of_float(sqrt((float (w*w) +. (float (h*h)))))in
  let mat = Array.make_matrix (lines + 1) 91 0 in
  let p_float = ref 0. in
  let p_int = ref 0 in
  for y = 0 to (h-1) do
    for x = 0 to (w-1) do
      if Sdlvideo.get_pixel_color img x y = (0, 0, 0) then
	for ang = 0 to 90 do
	  let an = (float ang) *. pi /. 180. in
          p_float := (float x)*.cos(an) +. (float y)*.sin(an);
          p_int := int_of_float(!p_float);
	  mat.(!p_int).(ang) <- mat.(!p_int).(ang) + 1;
	done;
    done;
  done;
  max_mat mat;;

let print_matrix_img src dst mat =
  let (x,y) = get_dims src in
  for i = 0 to x-1 do
    for j = 0 to y-1 do
      Sdlvideo.put_pixel_color dst i j mat.(i).(j)
    done
  done
    
let rec nb_white2blackX i j src =
  let (x,y) = get_dims src in
  let (r,g,b) = Sdlvideo.get_pixel_color src i j in
  if i < x-1 then
    if (r,g,b) > (200,200,200) then
      1 + nb_white2blackX (i+1) j src
    else
      0
  else
    0

let rec nb_white2blackY i j src =
  let (x,y) = get_dims src in
  let (r,g,b) = Sdlvideo.get_pixel_color src i j in
  if j < y-1 then
    if (r,g,b) > (200,200,200) then
      1 + nb_white2blackY i (j+1) src
    else
      0
  else
    0

let lissage_w src space =
  let (x,y) = get_dims src in
  let mat = Array.make_matrix x y (255,255,255) in
  for i=0 to x-1 do
    for j=0 to y-1 do
      let (r,g,b) = Sdlvideo.get_pixel_color src i j in
      if (r,g,b) != (255,255,255) && (nb_white2blackX i j src <= space) && (i+space < x-1) then
	let nextB = nb_white2blackX i j src + i in
	for n = i  to nextB do
	  mat.(n).(j) <- (0,0,0)
	done
    done
  done; mat

let lissage_h src space =
  let (x,y) = get_dims src in
  let mat = Array.make_matrix x y (255,255,255) in
  for i=0 to x-1 do
    for j=0 to y-1 do
      let (r,g,b) = Sdlvideo.get_pixel_color src i j in
      if (r,g,b) != (255,255,255) && (nb_white2blackY i j src <= space) && (j+space < y-1) then
	let nextB = nb_white2blackY i j src + j in
	for n = j  to nextB do
	  mat.(i).(n) <- (0,0,0)
	done
    done
  done; mat

let fun_or src m1 m2=
  let (x,y) = get_dims src in
  let mat = Array.make_matrix x y (255,255,255)in
  for i = 0 to x -1 do
    for j = 0 to y -1 do
      if m1.(i).(j) = (0,0,0) || m2.(i).(j) = (0,0,0) then
	mat.(i).(j) <- (0,0,0)
    done
  done; mat

let fun_and src m1 m2=
  let (x,y) = get_dims src in
  let mat = Array.make_matrix x y (255,255,255)in
  for i = 0 to x -1 do
    for j = 0 to y -1 do
      if m1.(i).(j) = (0,0,0) && m2.(i).(j) = (0,0,0) then
	mat.(i).(j) <- (0,0,0)
    done
  done; mat

let pretreatment =
  sdl_init ();
  (* Chargement d'une image *)
  let img = Sdlloader.load_image Sys.argv.(1) in
  (* On récupère les dimensions *)
  let (w,h) = get_dims img in
  (* On crée la surface d'affichage en doublebuffering *)
  let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
  (* on affiche l'image *)
  show img display;
  (* on attend une touche *)
  wait_key ();
  let dst = Sdlvideo.create_RGB_surface_format img [] w h in
  image2grey img dst;
  grey2black_white dst 0.75;
  Sdlvideo.save_BMP dst "inProgress";
  let dst2 = Sdlvideo.load_BMP "inProgress" in
  let ang = detect_rot dst2 - 90 in
  rot_img dst dst2 (float ang);
  remove_noise dst2;

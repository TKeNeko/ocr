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

(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
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
    let reseau = new Neuron.network 1 in
    reseau#initialize;
    image2grey img dst;
    show dst display;
    wait_key ();
    grey2black_white dst 0.75;
    show dst display;
    wait_key ();
    Sdlvideo.save_BMP dst "inProgress";
    let dst2 = Sdlvideo.load_BMP "inProgress" in
    let ang = detect_rot dst2 - 90 in
    rot_img dst dst2 (float ang);
    show dst2 display;
    wait_key ();
    remove_noise dst2;
    show dst2 display;
    wait_key ();
    let dst3 = Sdlvideo.create_RGB_surface_format dst2 [] w h in
    let matW = lissage_w dst2 60 in
    let matH = lissage_h dst2 60 in
    print_matrix_img dst2 dst3 (fun_and dst2 matW matH);
    show dst3 display;
    wait_key ();
    Sdlvideo.save_BMP dst3 "inProgress";
    (* on quitte *)
    exit 0
  end
    
let _ = main ()

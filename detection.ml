let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
    
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
    
let rec wait_key () =
  let e = Sdlevent.wait_event () in
  match e with
      Sdlevent.KEYDOWN _ -> ()
    | _ -> wait_key ()
      
let show img dst =
  let d = Sdlvideo.display_format img in
  Sdlvideo.blit_surface d dst ();
  Sdlvideo.flip dst
    
let img_to_matrix img = 
  let (w,h) = get_dims img in
  let mat =  Array.make_matrix w h 0 in
  for i=0 to w-1 do
    for j=0 to h-1 do
      let (grey,grey1,grey2) = Sdlvideo.get_pixel_color img i j in
      mat.(i).(j) <- grey
    done
  done; mat
    
let matrix_to_img img_matrix img =
  let w = Array.length img_matrix in
  let h = Array.length img_matrix.(0) in
  for i=0 to w-1 do
    for j=0 to h-1 do
      Sdlvideo.put_pixel_color img i j (img_matrix.(i).(j),img_matrix.(i).(j),
					img_matrix.(i).(j))
    done
  done
    
let mat_bin img = 
  let (w,h) = get_dims img in
  let mat = Array.make_matrix w h 0 in
  for i=0 to w-1 do
    for j=0 to h-1 do
      let (r,g,b) = Sdlvideo.get_pixel_color img i j in
      if (r,g,b) > (127,127,127) then
        mat.(i).(j) <- 1
    done
  done; mat
    
let print_matrix dst mat =
  let w = Array.length mat in
  let h = Array.length mat.(0) in
  for i=0 to w-1 do
    for j=0 to h-1 do
      if mat.(i).(j) = 1 then
	Sdlvideo.put_pixel_color dst i j (255,255,255)
      else if mat.(i).(j) = 2 then
	Sdlvideo.put_pixel_color dst i j (255,0,0)
      else
	Sdlvideo.put_pixel_color dst i j (0,0,0)
    done
  done
   
let check_brother mat =
  let tab = Array.make (Array.length mat.(0)) false in
  for j = 0 to (Array.length mat.(0) - 1) do
    for i = 0 to (Array.length mat - 1) do
      if (mat.(i).(j) = 0) then
	tab.(j) <- true
    done
  done;
  tab
    
let line2black mat y =
  for i = 0 to (Array.length mat - 1) do
    mat.(i).(y) <- 1
  done

let line2Red mat y =
  for i = 0 to (Array.length mat - 1) do
    mat.(i).(y) <- 2
  done

let search_line mat mat_s line=
  let tab = check_brother mat in
  let x = ref 0 in
  for j = 0 to Array.length tab -2 do
    if (tab.(j+1) && not tab.(j)) then begin
      line2black mat (j-1);
      line2Red mat_s (j-1);
      line.(!x) <- j;
      x := !x + 1
    end
    else if (not tab.(j+1) && tab.(j)) then begin
      line2black mat (j+1);
      line2Red mat_s (j+1);
      line.(!x) <- j;
      x := !x + 1
    end
  done

let check_sister mat x y=
  let tab = Array.make (Array.length mat) false in
  for i = 0 to (Array.length mat - 1) do
    for j = x to (y - 1) do
      if (mat.(i).(j) = 0) then
        tab.(i) <- true
    done
  done;
  tab
    
let colone2black mat x y1 y2 =
  for j = y1 to (y2) do
    mat.(x).(j) <- 1
  done
    
let colone2Red mat x y1 y2 =
  for j = y1 to (y2) do
    mat.(x).(j) <- 2
  done
    
let search_colone mat mat_s x y cL cR=
  let n = ref 0 in
  let m = ref 0 in
  let tab = check_sister mat (x) (y) in
  for i = 0 to Array.length tab -2 do
    if (tab.(i+1) && not tab.(i)) then begin
      colone2black mat (i) x y;
      colone2Red mat_s (i) x y;
      cL.(!n) <- i;
      n := !n + 1;
    end
    else if (not tab.(i+1) && tab.(i)) then begin
      colone2black mat (i) x y;
      colone2Red mat_s (i) x y;
      cR.(!m) <- i;
      m := !m + 1
    end
    else
      begin
	n := !n + 1;
	m := !m + 1
      end
  done
    
let put_colone mat mat_s l cL cR= 
  let ieme = ref (List.length l-1) in
  while (!ieme > 0) do
    let x = List.nth l !ieme in
    let y = List.nth l (!ieme - 1) in
    search_colone mat mat_s x y cL cR;
    ieme := !ieme - 2
  done
    
let create_liste mat =
  let liste = ref [] in
  for j = 0 to Array.length mat.(0) - 1 do
    if (mat.(0).(j) = (1)) then
      liste := (j) :: !liste
  done;
  !liste

let rec charP mat i j =
  let y =  Array.length mat.(0) - 1 in
  if j+1<y && mat.(i).(j) = 0 then
    1 + charP mat i (j+1)
  else if j+1<y && mat.(i).(j) = 1 then
    charP mat i (j+1)
  else
    0

let rec charM mat i j =
  let y =  Array.length mat.(0) - 1 in
  if y-1>0 && mat.(i).(j) = 0 then
    1 + charM mat i (j-1)
  else if j-1>0 && mat.(i).(j) = 1 then
    charM mat i (j-1)
  else
    0

let rect mat =
  let w = Array.length mat in
  let h = Array.length mat.(0) in
  let mat_s = mat in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      if mat.(i).(j) = 2 && mat.(i).(j+1) = 1 && mat.(i).(j-1) = 1 && charP mat i (j+1) = 0 && charM mat i (j-1) = 0 then
	mat_s.(i).(j) <- 1
    done
  done; mat_s

let affiche tab =
  begin
  for i = 0 to Array.length tab - 1 do
    print_int tab.(i);
    print_string " "
  done;
    print_newline()
  end

let taille tab =
  let w = Array.length tab in
  let rec aux tab i =
    if (i< w-1) && tab.(i) != 0 then
      1 + aux tab (i+1)
    else if (i<w-1) && tab.(i) =0 then
      aux tab (i+1)
    else
      0
  in aux tab 0

let suppZero tab =
  let newTab = Array.make (taille tab) 0 in
  let n = ref 0 in
    for i = 0 to Array.length tab - 1 do
      if tab.(i) != 0 then
	begin
	  newTab.(!n) <- tab.(i);
	  n := !n + 1
	end
    done; newTab

let quatior mat l cL cR =
  let tab = Array.make (Array.length cL) (0,0,0,0) in
  let n = ref 0 in
  for i = 0 to Array.length cL - 1 do
    for j = 0 to Array.length l - 2 do
	if mat.(cL.(i)).(l.(j)) = 2 && mat.(cR.(i)).(l.(j+1)) = 2 then
	  begin
	    tab.(!n) <- (cL.(i),l.(j),cR.(i),l.(j+1));
	    n := !n +1
	  end
    done
  done; tab

let affiche_q tab =
  for i = 0 to Array.length tab - 1 do
    let print (a,b,c,d) =
      print_newline();
      print_string "(";
      print_int a;
      print_string ",";
      print_int b;
      print_string ",";
      print_int c;
      print_string ",";
      print_int d;
      print_string ")";
    in print tab.(i)
  done

let found_char img =
  begin
    let (w,h) = get_dims img in
    let img_mat = img_to_matrix img in
    let mat_s = mat_bin img in   
    let tab_line = Array.make (h-1) 0 in
    let tab_coloneL = Array.make (w-1) 0 in
    let tab_coloneR = Array.make (w-1) 0 in
    search_line img_mat mat_s tab_line;  
    let liste = create_liste img_mat in
    put_colone img_mat mat_s liste tab_coloneL tab_coloneR; 
    
    rect mat_s
  end

let found_quatior img =
  begin
    let (w,h) = get_dims img in
    let img_mat = img_to_matrix img in
    let mat_s = mat_bin img in 
    let tab_line = Array.make (h-1) 0 in
    let tab_coloneL = Array.make (w-1) 0 in
    let tab_coloneR = Array.make (w-1) 0 in
    search_line img_mat mat_s tab_line;   
    let liste = create_liste img_mat in
    put_colone img_mat mat_s liste tab_coloneL tab_coloneR;
    let line = suppZero tab_line in
    let cL = suppZero tab_coloneL in
    let cR = suppZero tab_coloneR in   
    let mat_rect = rect mat_s in
    quatior mat_rect line cL cR
  end
    
(* main
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
    wait_key();
    
    show img display;
    wait_key();
    (* on quitte *)
    exit 0
  end
    
let _ = main ()
*)

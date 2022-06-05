open Graphics

type view = {
    min_x: float;
    min_y: float;
    max_x: float;
    max_y: float;
  }

let to_view_x view x = truncate ((x -. view.min_x) /. (view.max_x -. view.min_x) *. float (size_x ()))

let to_view_y view y = truncate ((y -. view.min_y) /. (view.max_y -. view.min_y) *. float (size_y ()))

type scene = {
    player_x: float;
    player_y: float;
    player_speed: float;
    player_angle: float;
  }

let total = ref 0
let blues = ref 0

let draw_scene view scene =
  clear_graph ();
  let t = 4 in
  let int_player_x = truncate scene.player_x in
  let int_player_y = truncate scene.player_y in
  for x = int_player_x - t to int_player_x + t do
    for y = int_player_y - t to int_player_y + t do
      let view_x = to_view_x view (float x) in
      let view_y = to_view_y view (float y) in
      let view_w = to_view_x view (float (x+1)) - view_x in
      let view_h = to_view_y view (float (y+1)) - view_y in
      let d =
        if x = int_player_x then
          if y = int_player_y then 0.
          else if y < int_player_y then scene.player_y -. (float y +. 1.)
          else float y -. scene.player_y
        else if x < int_player_x then
          if y = int_player_y then scene.player_x -. (float x +. 1.)
          else if y < int_player_y then sqrt ((scene.player_x -. (float x +. 1.)) ** 2. +. (scene.player_y -. (float y +. 1.)) ** 2.)
          else sqrt ((scene.player_x -. (float x +. 1.)) ** 2. +. (float y -. scene.player_y) ** 2.)
        else if y = int_player_y then float x -. scene.player_x
        else if y < int_player_y then sqrt ((float x -. scene.player_x) ** 2. +. (scene.player_y -. (float y +. 1.)) ** 2.)
        else sqrt ((float x -. scene.player_x) ** 2. +. (float y -. scene.player_y) ** 2.) in
      let is_blue = d < float t in
      set_color (if is_blue then blue else red);
      fill_rect view_x view_y view_w view_h;
      total := !total + 1;
      if is_blue then
        blues := !blues + 1
    done
  done;
  set_color black;
  moveto 5 (size_y () - 15);
  draw_string (Printf.sprintf "total = %d blue = %d (%.1f%%)" !total !blues (100. *. float !blues /. float !total));
  for y = truncate (view.min_y) to truncate (view.max_y) do
    moveto 0 (to_view_y view (float y));
    lineto (size_x ()) (to_view_y view (float y))
  done;
  for x = truncate (view.min_x) to truncate (view.max_x) do
    moveto (to_view_x view (float x)) 0;
    lineto (to_view_x view (float x)) (size_y ())
  done;
  let view_player_x = to_view_x view scene.player_x in
  let view_player_y = to_view_y view scene.player_y in
  plot view_player_x view_player_y;
  let view_radius = (to_view_x view (scene.player_x +. float t)) - view_player_x in
  draw_circle view_player_x view_player_y view_radius;
  synchronize ()
   
let rec loop scene =
  let scale = 50. in
  let view = {
      min_x = scene.player_x -. float (size_x ()) /. 2. /. scale;
      min_y = scene.player_y -. float (size_y ()) /. 2. /. scale;
      max_x = scene.player_x +. float (size_x ()) /. 2. /. scale;
      max_y = scene.player_y +. float (size_y ()) /. 2. /. scale;
    } in
  draw_scene view scene;
  Unix.sleepf (1. /. 60.);
  if not (key_pressed ()) then
    let angle = scene.player_angle -. 0.03 +. Random.float 0.06 in
    loop {
        scene with
        player_x = scene.player_x +. cos scene.player_angle *. scene.player_speed;
        player_y = scene.player_y +. sin scene.player_angle *. scene.player_speed;
        player_angle = angle; }
   
let () =
  open_graph " 1024x768";
  auto_synchronize false;
  let angle = Random.float (Float.pi *. 2.) in
  loop { player_x = 1000.5; player_y = 1000.5; player_speed = 0.005; player_angle = angle };
  close_graph ()

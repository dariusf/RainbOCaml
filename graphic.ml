open Graphics
open Colors

let border = 5
let side = 120
let size = 3 * side + 4 * border

let mode = ref Addition

let not_mode = function
  |Addition -> Substract
  |Substract -> Addition

let get_color = function
  | Blue -> blue
  | Red -> red
  | Green -> green
  | Cyan -> cyan
  | Yellow -> yellow
  | Magenta -> magenta
  | White -> white

let draw_square x y case =
  let fill () =
    fill_rect ((succ x) * border + x * side)
      ((succ y) * border + y * side)
    side side
  in
  match case with
  | Color color -> set_color (get_color color); fill ()
  | Empty -> ()

let draw_background () =
  if !mode = Addition then
    begin
      set_color black;
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color (rgb 128 128 128)
    end
  else
    begin
      set_color (rgb 128 128 128);
      fill_rect 0 0 (size_x ()) (size_y ());
      set_color black
    end;
  for a = 0 to 3 do
    fill_rect 0 (a * (border + side)) size border;
    fill_rect (a * (border + side)) 0  border size
  done

let display_grid grid =
  draw_background ();
  Array.iteri (fun x line -> Array.iteri (draw_square x) line) grid

let get_mode () = match !mode with
  | Substract -> sub
  | Addition -> add

let move key grid =
  begin
    match key with
    |'2' -> fusion (get_mode ()) Down grid
    |'4' -> fusion (get_mode ()) Left grid
    |'6' -> fusion (get_mode ()) Right grid
    |'8' -> fusion (get_mode ()) Up grid
    | _ -> ()
  end;
  display_grid grid;
  Random.self_init ();
  if ((Random.int 3) mod 3) = 0
  then mode := (not_mode !mode)

let () =
  open_graph (Printf.sprintf " %dx%d" size size);
  let grid = init_grid () in
  display_grid grid;
  loop_at_exit [Key_pressed] (fun event -> move event.key grid)

type color =      Blue
                | Red
                | Green
                | Yellow
                | Cyan
                | Magenta
                | White

type case = Empty
          |  Color of color

type mode =       Substract
                | Addition

type direction =  Up
                | Down
                | Left
                | Right

type result =  Merge of color
            | Move of color
            | Nothing
            | Failed
            | Win

let primary_color = [Red;Blue;Green]

let is_empty = function
  |Empty -> true
  |_ -> false

let init_random_case grid =
    Random.self_init ();
    let rec place n = match grid.(n mod 3).(n/3) with
      | Empty ->
          grid.(n mod 3).(n/3) <-
            Color (List.nth primary_color (Random.int 3));
          ()
      | _ -> place ((n+1) mod 9)
    in place (Random.int 8)


let init_grid () =
    let grid = Array.create_matrix 3 3 Empty in
    init_random_case grid;
    init_random_case grid;
    grid

let sub c1 c2 = match c1, c2 with
  | Color cc1, Color cc2 -> begin
      match cc1, cc2 with
      | Magenta, Cyan
      | Cyan, Magenta -> Merge Blue
      | Yellow, Cyan
      | Cyan, Yellow -> Merge Green
      | Magenta, Yellow
      | Yellow, Magenta -> Merge Red
      | _,_ -> Failed
    end
  | Empty, Color cc1 -> Move cc1
  | _,_ -> Nothing


let add c1 c2 = match c1, c2 with
  | Color cc1, Color cc2 -> begin
      match cc1, cc2 with
      | Red, Blue
      | Blue, Red -> Merge Magenta
      | Green, Blue
      | Blue, Green -> Merge Cyan
      | Red, Green
      | Green, Red -> Merge Yellow
      | Magenta, Green
      | Green, Magenta
      | Yellow, Blue
      | Blue, Yellow
      | Red, Cyan
      | Cyan, Red -> Win
      | _,_ -> Failed
    end
  | Empty, Color cc1 -> Move cc1
  | _,_ -> Nothing

let get_pos direction (x,y) = match direction with
  | Left -> (x, y)
  | Down -> (y, x)
  | Up -> (y, (2 - x))
  | Right -> ((2 - x), y)

let fusion f direction grid =
  let rec loop2 (px, py) (x,y) =
    if (px = x) ||  (x > 2) then begin loop1 (succ y) end
    else
      begin
        let (rpx, rpy) = get_pos direction (px, py) in
        let (rcx, rcy) = get_pos direction (x, y) in
        let rad = f grid.(rpx).(rpy) grid.(rcx).(rcy) in
        match rad with
        | Merge nc ->
            begin
              grid.(rcx).(rcy) <- Empty;
              grid.(rpx).(rpy) <- Color nc;
              loop2 (succ px, py) ((succ x),y)
            end
        | Move nc ->
            begin
              grid.(rcx).(rcy) <- Empty;
              grid.(rpx).(rpy) <- Color nc;
              loop2 (px, py) ((succ x),y)
            end
        | Failed ->
            if (px + 1) < x then
              loop2 (succ px, py) (x,y)
            else
              loop2 (succ px, py) ((succ x),y)
        | Nothing -> loop2 (px, py) ((succ x),y)
        | Win ->
            begin
              grid.(rcx).(rcy) <- Empty;
              grid.(rpx).(rpy) <- Color White;
              loop2 (succ px, py) ((succ x),y)
            end
      end
  and loop1 y =
    if y < 3 then loop2 (0,y) (1,y)
  in
  loop1 0;
  let libre = Array.fold_left
                (fun acc line -> Array.fold_left
                                   (fun acc2 slab -> if is_empty slab
                                     then succ acc2
                                     else acc2)
                                   acc line)
                0 grid
  in
  if libre <> 0 then  ignore (init_random_case grid)
  else print_endline "lost"

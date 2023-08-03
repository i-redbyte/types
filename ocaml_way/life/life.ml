(* Определение размера сетки *)
let width = 10
let height = 10

(* Инициализация сетки случайными значениями *)
let grid = Array.make_matrix width height false

let rec randomize_grid x y =
  if x < width && y < height then begin
    grid.(x).(y) <- Random.bool ();
    randomize_grid (x+1) (if x+1 < width then y else y+1)
  end

(* Вывод сетки в консоль *)
let print_grid () =
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      print_char (if grid.(x).(y) then 'X' else '.')
    done;
    print_newline ()
  done

(* Проверка соседей клетки *)
let count_neighbors x y =
  let count = ref 0 in
  for i = -1 to 1 do
    for j = -1 to 1 do
      let neighbor_x = (x + i + width) mod width in
      let neighbor_y = (y + j + height) mod height in
      if grid.(neighbor_x).(neighbor_y) then count := !count + 1
    done
  done;
  if grid.(x).(y) then count := !count - 1;
  !count

(* Обновление сетки на следующий шаг *)
let update_grid () =
  let new_grid = Array.make_matrix width height false in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let neighbors = count_neighbors x y in
      if grid.(x).(y) && (neighbors = 2 || neighbors = 3) then
        new_grid.(x).(y) <- true
      else if not grid.(x).(y) && neighbors = 3 then
        new_grid.(x).(y) <- true
    done
  done;
  new_grid

(* Запуск игры *)
let rec run_game () =
  randomize_grid 0 0;
  print_grid ();
  let new_grid = update_grid () in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      grid.(x).(y) <- new_grid.(x).(y)
    done
  done;
  print_grid ();
  run_game ()

(* Запуск игры *)
let () =
  Random.self_init ();
  run_game ()

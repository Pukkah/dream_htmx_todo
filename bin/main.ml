type todo =
  { id : int
  ; name : string
  ; mutable completed : bool
  }

let todo_checkbox todo =
  let open Dream_html in
  let open HTML in
  input
    [ type_ "checkbox"
    ; class_ "toggle"
    ; Hx.patch "/todo/%n" todo.id
    ; Hx.swap "outerHTML"
    ; (if todo.completed then checked else null_)
    ]
;;

let todo_item todo =
  let open Dream_html in
  let open HTML in
  li
    []
    [ div
        [ class_ "view" ]
        [ todo_checkbox todo
        ; label [] [ txt "%n - %s" todo.id todo.name ]
        ; button
            [ class_ "destroy"
            ; Hx.delete "/todo/%n" todo.id
            ; Hx.target "closest li"
            ; Hx.swap "outerHTML"
            ]
            []
        ]
    ]
;;

let todo_list todos =
  let open Dream_html in
  let open HTML in
  ul [ class_ "todo-list" ] @@ List.map todo_item todos
;;

let index req todos =
  let open Dream_html in
  let open HTML in
  html
    [ lang "eng" ]
    [ head
        []
        [ meta [ name "viewport"; content "width=device-width, initial-scale=1" ]
        ; title [] "hello-dream • TodoMVC"
        ; link [ rel "stylesheet"; href "/static/styles.css" ]
        ; script [ src "https://unpkg.com/htmx.org@1.9.4" ] ""
        ]
    ; body
        []
        [ section
            [ class_ "todoapp" ]
            [ header
                [ class_ "header" ]
                [ h1 [] [ txt "Hello, Dream" ]
                ; form
                    [ Hx.put "/todo"; Hx.target ".todo-list"; Hx.swap "beforeend" ]
                    [ csrf_tag req
                    ; input
                        [ class_ "new-todo"
                        ; name "name"
                        ; placeholder "What needs to be done?"
                        ; autofocus
                        ]
                    ]
                ]
            ; section [ class_ "main" ] [ todo_list todos ]
            ]
        ]
    ]
;;

let () =
  let count = ref 0 in
  let todos = ref [] in
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/static/**" (Dream.static "./static")
       ; Dream.get "/" (fun req -> Dream_html.respond (index req !todos))
       ; Dream.put "/todo" (fun request ->
           let open Lwt.Syntax in
           let* form = Dream.form request in
           match form with
           | `Ok [ ("name", name) ] when name |> String.trim |> String.length > 0 ->
             count := !count + 1;
             let todo = { id = !count; name; completed = false } in
             todos := !todos @ [ todo ];
             Dream_html.respond @@ todo_item todo
           | _ -> Dream.empty `Bad_Request)
       ; Dream.patch "/todo/:id" (fun request ->
           let id = Dream.param request "id" |> int_of_string in
           let todo = List.find (fun x -> x.id = id) !todos in
           todo.completed <- not todo.completed;
           Dream_html.respond @@ todo_checkbox todo)
       ; Dream.delete "/todo/:id" (fun req ->
           let id = Dream.param req "id" |> int_of_string in
           todos := List.filter (fun todo -> todo.id != id) !todos;
           Dream.empty `OK)
       ]
;;

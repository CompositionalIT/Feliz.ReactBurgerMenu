module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Feliz.ReactBurgerMenu

type Model = { Todos: Todo list; Input: string; Menu: bool; InputText : string; }

type Person = { Name: string; Age: int;  }

let changeAge (person : Person) (age : int) =
    { person with Age = age }

let changeName (person : Person) (name : string) =
    { person with Name = name }





type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | ToggleMenu
    | SetInputText of string

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = ""; Menu = false; InputText = "" }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none

    | ToggleMenu ->
        { model with Menu = not model.Menu }, Cmd.none
    | SetInputText value ->
        { model with InputText = value }, Cmd.none


open Feliz
open Feliz.Bulma
open Fable.Core.JsInterop

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]


let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ Html.div $"{model.Menu}" ]
                ]
            ]





            Html.div [
                prop.children [
                    BurgerMenu.create [
                        BurgerMenu.onOpen (fun _ -> dispatch ToggleMenu)
                        BurgerMenu.onClose (fun _ -> dispatch ToggleMenu)
                        BurgerMenu.disableOverlayClick false
                        BurgerMenu.isOpen model.Menu
                        BurgerMenu.styles [
                            Menu [ style.width "300px" ]
                        ]
                        BurgerMenu.animation Bubble
                        BurgerMenu.children [
                            Html.div "Hello"
                            Html.div "Hello"
                            Html.div "Hello"
                        ]
                    ]
                ]
            ]
        ]



            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Feliz.ReactBurgerMenu"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]


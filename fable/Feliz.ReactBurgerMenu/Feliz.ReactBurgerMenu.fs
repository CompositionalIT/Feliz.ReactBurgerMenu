module Feliz.ReactBurgerMenu

open Fable.Core.JsInterop
open Feliz

let bubble: obj = import "bubble" "react-burger-menu"
let slide : obj = import "slide" "react-burger-menu"
let stack : obj = import "stack" "react-burger-menu"
let elastic : obj = import "elastic" "react-burger-menu"
let push : obj = import "push" "react-burger-menu"
let pushRotate : obj = import "pushRotate" "react-burger-menu"
let scaleDown : obj = import "scaleDown" "react-burger-menu"
let scaleRotate : obj = import "scaleRotate" "react-burger-menu"
let fallDown : obj = import "fallDown" "react-burger-menu"
let reveal : obj = import "reveal" "react-burger-menu"


type Animation =
    | Bubble
    | Stack
    | Elastic
    | Slide
    | Push
    | PushRotate
    | ScaleDown
    | ScaleRotate
    | FallDown
    | Reveal

        static member importAnimation = function
            | Bubble -> bubble
            | Stack -> stack
            | Elastic -> elastic
            | Slide -> slide
            | Push -> push
            | PushRotate -> pushRotate
            | ScaleDown -> scaleDown
            | ScaleRotate -> scaleRotate
            | FallDown -> fallDown
            | Reveal -> reveal

type Dimension =
    | Int of int
    | Percent of int
    | Pixel of int

type MenuPosition = Left | Right

type IReactBurgerMenuProperty = interface end

type Style =
    | Menu of IStyleAttribute list
    | BurgerButton of IStyleAttribute list
    | BurgerBars of IStyleAttribute list
    | BurgerBarsHover of IStyleAttribute list
    | CrossButton of IStyleAttribute list
    | Cross of IStyleAttribute list
    | MenuWrap of IStyleAttribute list
    | MorphShape of IStyleAttribute list
    | ItemList of IStyleAttribute list
    | Item of IStyleAttribute list
    | Overlay of IStyleAttribute list

        static member toStyleName = function
            | Menu _ -> "bmMenu"
            | BurgerButton _ -> "bmBurgerButton"
            | BurgerBars _ -> "bmBurgerBars"
            | BurgerBarsHover _ -> "bmBurgerBarsHover"
            | CrossButton _ -> "bmCrossButton"
            | Cross _ -> "bmCross"
            | MenuWrap _ -> "bmMenuWrap"
            | MorphShape _ -> "bmMorphShape"
            | ItemList _ -> "bmItemList"
            | Item _ -> "bmItem"
            | Overlay _ -> "bmOverlay"

        static member unwrap = function
            | Menu s
            | BurgerButton s
            | BurgerBars s
            | BurgerBarsHover s
            | CrossButton s
            | Cross s
            | MenuWrap s
            | MorphShape s
            | ItemList s
            | Item s
            | Overlay s -> s

let toStyleObject (styles: IStyleAttribute list) =
    styles
    |> List.toArray
    |> fun x -> createObj !!x

type BurgerMenu =

    static member inline width (width: Dimension) =
        match width with
        | Int i -> unbox<IReactBurgerMenuProperty>("width" ==> i)
        | Percent p ->
            let percentValue = sprintf "%i%%" p
            unbox<IReactBurgerMenuProperty>("width" ==> percentValue)
        | Pixel p ->
            let pixelValue = sprintf "%ipx" p
            unbox<IReactBurgerMenuProperty>("width" ==> pixelValue)


    static member inline bodyClassName (bodyClassName: string) = unbox<IReactBurgerMenuProperty>("bodyClassName" ==> bodyClassName)
    static member inline className (className: string) = unbox<IReactBurgerMenuProperty>("className" ==> className)
    static member inline burgerBarClassName (burgerBarClassName: string) = unbox<IReactBurgerMenuProperty>("burgerBarClassName" ==> burgerBarClassName)

    static member inline burgerButtonClassName (burgerButtonClassName: string) = unbox<IReactBurgerMenuProperty>("burgerButtonClassName" ==> burgerButtonClassName)
    static member inline crossButtonClassName (crossButtonClassName: string) = unbox<IReactBurgerMenuProperty>("crossButtonClassName" ==> crossButtonClassName)
    static member inline crossClassName (crossClassName: string) = unbox<IReactBurgerMenuProperty>("crossClassName" ==> crossClassName)
    static member inline itemClassName (itemClassName: string) = unbox<IReactBurgerMenuProperty>("itemClassName" ==> itemClassName)
    static member inline itemListClassName (itemListClassName: string) = unbox<IReactBurgerMenuProperty>("itemListClassName" ==> itemListClassName)
    static member inline menuClassName (menuClassName: string) = unbox<IReactBurgerMenuProperty>("menuClassName" ==> menuClassName)
    static member inline morphShapeClassName (morphShapeClassName: string) = unbox<IReactBurgerMenuProperty>("morphShapeClassName" ==> morphShapeClassName)
    static member inline overlayClassName (overlayClassName: string) = unbox<IReactBurgerMenuProperty>("overlayClassName" ==> overlayClassName)

    static member inline styles (styles: Style list) =
        let styleObject =
            styles
            |> List.fold (fun acc style ->
                let styleName = style |> Style.toStyleName
                let styleObject = style |> Style.unwrap |> toStyleObject
                (styleName ==> styleObject) :: acc
            ) List.empty

        unbox<IReactBurgerMenuProperty>("styles" ==> (createObj styleObject))


    static member inline disableAutoFocus (disableAutoFocus: bool) = unbox<IReactBurgerMenuProperty>("disableAutoFocus" ==> disableAutoFocus)
    static member inline disableCloseOnEsc (disableCloseOnEsc: bool) = unbox<IReactBurgerMenuProperty>("disableCloseOnEsc" ==> disableCloseOnEsc)
    static member inline noOverlay (noOverlay: bool) = unbox<IReactBurgerMenuProperty>("noOverlay" ==> noOverlay)
    static member inline noTransition (noTransition: bool) = unbox<IReactBurgerMenuProperty>("noTransition" ==> noTransition)
    static member inline isOpen (isOpen: bool) = unbox<IReactBurgerMenuProperty>("isOpen" ==> isOpen)
    static member inline disableOverlayClick (disableOverlayClick: bool) = unbox<IReactBurgerMenuProperty>("disableOverlayClick" ==> disableOverlayClick)

    static member inline onOpen (onOpen: (_ -> unit)) = unbox<IReactBurgerMenuProperty>("onOpen" ==> onOpen)
    static member inline onClose (onClose: (_ -> unit)) = unbox<IReactBurgerMenuProperty>("onClose" ==> onClose)


    static member customBurgerIcon (icon: ReactElement option) =
        match icon with
        | Some e -> unbox<IReactBurgerMenuProperty>("customBurgerIcon" ==> e)
        | None -> unbox<IReactBurgerMenuProperty>("customBurgerIcon" ==> false)

    static member customCrossIcon (icon: ReactElement option) =
        match icon with
        | Some e -> unbox<IReactBurgerMenuProperty>("customCrossIcon" ==> e)
        | None -> unbox<IReactBurgerMenuProperty>("customCrossIcon" ==> false)

    static member inline menuPosition (menuPosition: MenuPosition) =
        match menuPosition with
        | Left -> unbox<IReactBurgerMenuProperty>("right" ==> false)
        | Right -> unbox<IReactBurgerMenuProperty>("right" ==> true)


    static member inline animation (animationType: Animation) =
        let import = animationType |> Animation.importAnimation
        unbox<IReactBurgerMenuProperty>("animation" ==> import)

    static member inline pageWrapId (id: string) = unbox<IReactBurgerMenuProperty>("pageWrapId" ==> id)
    static member inline outerContainerId (id: string) = unbox<IReactBurgerMenuProperty>("outerContainerId" ==> id)

    static member inline children (e: ReactElement list) = unbox<IReactBurgerMenuProperty>(prop.children e)

    static member inline create props =
        let givenProps = !!props

        let importType =
            givenProps
            |> Seq.tryFind (fst >> (=) "animation")
            |> function
                | Some x -> snd x
                | None -> slide

        Interop.reactApi.createElement (importType, createObj !!props)
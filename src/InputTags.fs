module Fulma.Extensions.InputTags

open Feliz
open Browser

open Zanaptak.TypedCssClasses
open Fable.FontAwesome

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css", Naming.PascalCase>

type State =
    {
        CurrentTag: string
        IsActive: bool
        SelectedTag: int
        Tags: string list
        InputTag: string
    }
    static member Empty =
        {
            CurrentTag = ""
            IsActive = false
            SelectedTag = -1
            Tags = []
            InputTag = ""
        }

let dropdown state addTag changeState items =
    Html.div [
        prop.className [
            Bulma.DropdownMenu
        ]
        if state.IsActive then
            prop.style [
                style.display.block
            ]
        prop.custom("id", Bulma.DropdownMenu)
        prop.custom("role", "menu")
        prop.children [
            Html.div [
                prop.className [
                    Bulma.DropdownContent
                ]
                items
                |> Array.mapi (fun i (tag:string) ->
                    Html.a [
                        let isSelected = state.SelectedTag = i
                        if isSelected then
                            prop.ref (fun x ->
                                if not <| isNull x then
                                    if state.IsActive then
                                        let x = x :?> Types.HTMLElement
                                        x.focus ()
                            )
                            // prop.autoFocus true
                            prop.onKeyDown (fun e ->
                                match e.key with
                                | "ArrowDown" ->
                                    if i + 1 < items.Length then
                                        { state with
                                            SelectedTag = i + 1
                                        }
                                        |> changeState
                                    else
                                        { state with
                                            SelectedTag = -1
                                        }
                                        |> changeState
                                | "ArrowUp" ->
                                    if i - 1 < 0 then
                                        { state with
                                            SelectedTag = -1
                                        }
                                        |> changeState
                                    else
                                        { state with
                                            SelectedTag = i - 1
                                        }
                                        |> changeState
                                | "Enter" ->
                                    let newState =
                                        { state with
                                            CurrentTag = ""
                                            SelectedTag = -1
                                        }
                                    (newState, tag)
                                    |> addTag
                                | _ -> ()
                            )
                        prop.tabIndex -1 // necessarily prop
                        prop.className [
                            Bulma.DropdownItem
                            if isSelected then
                                Bulma.IsActive
                        ]
                        prop.children [
                            Html.text tag
                        ]
                        prop.onClick (fun _ ->
                            let newState =
                                { state with
                                    CurrentTag = ""
                                    SelectedTag = -1
                                }
                            (newState, tag)
                            |> addTag
                        )
                    ]
                )
                |> prop.children
            ]
        ]
    ]
let inputTags (inputId:string) setInputTagsState getSuggestions suggestions (state:State) =
    let createTag (name:string) =
        Html.div [
            prop.className [
                Bulma.Control
            ]
            prop.style [
                style.marginBottom (length.em 0.1)
                style.marginTop (length.em 0.1)
            ]
            prop.custom ("data-tag", name)
            prop.children [
                Html.div [
                    prop.className [
                        Bulma.Tags
                        Bulma.HasAddons
                    ]
                    prop.children [
                        Html.span [
                            prop.className [
                                Bulma.Tag
                                Bulma.IsActive
                            ]
                            prop.text name
                        ]
                        Html.a [
                            prop.className [
                                Bulma.Tag
                                Bulma.IsDelete
                            ]
                            prop.onClick (fun _ ->
                                { state with
                                    Tags = state.Tags |> List.filter ((<>) name)
                                }
                                |> setInputTagsState
                            )
                        ]
                    ]
                ]
            ]
        ]
    let inputFieldId = "inputFieldId"
    let changeState state =
        state |> setInputTagsState
        state.CurrentTag |> getSuggestions
    let addTag (state, tag) =
        { state with
            Tags =
                List.foldBack
                    (fun x st ->
                        if x = tag then st
                        else x::st
                    )
                    state.Tags
                    [tag]
        }
        |> setInputTagsState

    Html.div [
        prop.id inputFieldId
        prop.className [
            Bulma.Field
            Bulma.IsGrouped
            Bulma.IsGroupedMultiline
            Bulma.Input
            if state.IsActive then
                Bulma.IsActive
        ]
        prop.style [
            style.height length.auto
        ]

        if not state.IsActive then
            prop.onClick (fun e ->
                if e.currentTarget = e.target then
                    // let x = document.getElementById inputId
                    // x.focus()
                    { state with
                        IsActive = true
                    }
                    |> changeState
            )

        prop.children [
            yield! Seq.map createTag state.Tags

            Html.div [
                prop.className [
                    Bulma.Field
                    Bulma.HasAddons
                ]
                prop.children [
                    Html.div [
                        prop.className [
                            Bulma.Control
                        ]
                        prop.children [
                            Html.input [
                                prop.id inputId
                                prop.type' "text"
                                if state.CurrentTag = "" then
                                    prop.placeholder "Add Tag"
                                else
                                    prop.value state.CurrentTag

                                prop.classes [
                                    Bulma.Input
                                    if state.IsActive then
                                        Bulma.IsActive
                                ]
                                prop.style [
                                    style.width 172
                                    style.marginBottom (length.em 0.1)
                                    style.marginTop (length.em 0.1)
                                ]
                                prop.ref (fun x ->
                                    if not <| isNull x then
                                        if state.IsActive && state.SelectedTag = -1 then
                                            let x = x :?> Types.HTMLElement
                                            x.focus ()
                                )
                                if state.IsActive then
                                    prop.onBlur (fun e ->
                                        match e.relatedTarget :?> Types.HTMLElement with
                                        | null ->
                                            { state with
                                                IsActive = false
                                            }
                                            |> changeState
                                        | related ->
                                            if related.classList.contains Bulma.DropdownItem then
                                                ()
                                            else
                                                { state with
                                                    IsActive = false
                                                }
                                                |> changeState
                                    )
                                else
                                    prop.onFocus (fun _ ->
                                        { state with
                                            IsActive = true
                                        }
                                        |> changeState
                                    )

                                // prop.spellcheck true // for some reason it doesn't work
                                prop.custom ("spellCheck", true)
                                prop.onTextChange (fun (tagName:string) ->
                                    { state with
                                        CurrentTag = tagName
                                    }
                                    |> changeState
                                )

                                prop.onKeyDown (fun e ->
                                    if not <| System.String.IsNullOrWhiteSpace state.CurrentTag then
                                        match e.key with
                                        | "Enter" ->
                                            let e = e.target :?> Types.HTMLInputElement
                                            let x = e.parentNode :?> Types.HTMLDivElement
                                            x.classList.remove Bulma.IsActive

                                            let newState =
                                                { state with
                                                    CurrentTag = ""
                                                    SelectedTag = -1
                                                }
                                            (newState, state.CurrentTag)
                                            |> addTag
                                        | "ArrowDown" ->
                                            if Array.length suggestions > 0 then
                                                { state with
                                                    SelectedTag = 0
                                                }
                                                |> changeState
                                        | "ArrowUp" ->
                                            { state with
                                                SelectedTag = suggestions.Length - 1
                                            }
                                            |> changeState
                                        | x ->
                                            printfn "key: %A" x
                                )
                            ]
                            match suggestions with
                            | [||] -> ()
                            | suggestions ->
                                dropdown state addTag changeState suggestions
                        ]
                    ]
                    Html.div [
                        prop.className [
                            Bulma.Control
                        ]
                        prop.children [
                            Html.a [
                                prop.className [
                                    Bulma.Button
                                ]
                                prop.style [
                                    style.marginBottom (length.em 0.1)
                                    style.marginTop (length.em 0.1)
                                ]
                                prop.onClick (fun _ ->
                                    if not <| System.String.IsNullOrWhiteSpace state.CurrentTag then
                                        let newState =
                                            { state with
                                                IsActive = true
                                                CurrentTag = ""
                                                SelectedTag = -1
                                            }
                                        (newState, state.CurrentTag)
                                        |> addTag
                                )
                                prop.children [
                                    Fa.i [ Fa.Solid.Check ] []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
namespace Superprojekt

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.Dom

module Gui =

    let burgerButton (env : Env<Message>) =
        button {
            Attribute("id", "burger-btn")
            Class "burger-btn"
            Dom.OnClick(fun _ -> env.Emit [ToggleMenu])
            div { Class "burger-line" }
            div { Class "burger-line" }
            div { Class "burger-line" }
        }

    let hudTabs (env : Env<Message>) (model : AdaptiveModel) =
        div {
            Class "tabs"
            model.MenuOpen |> AVal.map (fun o ->
                if o then Some (Class "tabs-open") else None
            )

            div {
                Class "tab-labels"
                label {
                    input {
                        Attribute("type",    "radio")
                        Attribute("name",    "hud-tabs")
                        Attribute("id",      "hud-tab1")
                        Attribute("checked", "checked")
                    }
                    "Scene"
                }
                label {
                    input {
                        Attribute("type", "radio")
                        Attribute("name", "hud-tabs")
                        Attribute("id",   "hud-tab2")
                    }
                    "Info"
                }
                label {
                    input {
                        Attribute("type", "radio")
                        Attribute("name", "hud-tabs")
                        Attribute("id",   "hud-tab3")
                    }
                    "Settings"
                }
            }

            div {
                Class "tab-panels"

                // ── Tab 1: Scene ──────────────────────────────────
                div {
                    Class "tab-panel"
                    Attribute("id", "hud-panel1")

                    h1 {
                        "Counter: "
                        model.Value |> AVal.map string
                    }
                    button { "+"; Dom.OnClick(fun _ -> env.Emit [Increment]) }
                    button { "-"; Dom.OnClick(fun _ -> env.Emit [Decrement]) }
                    button { "Clear Filter"; Dom.OnClick(fun _ -> env.Emit [ClearFilteredMesh]) }

                    h2 {
                        model.Hover |> AVal.map (function
                            | Some p -> "Hover: " + p.ToString("0.00")
                            | None   -> "Hover: none"
                        )
                    }

                    h3 { "Meshes" }
                    model.MeshNames |> AList.map (fun name ->
                        let isVis =
                            model.MeshVisible
                            |> AVal.map (fun m -> Map.tryFind name m |> Option.defaultValue true)
                        div {
                            label {
                                input {
                                    Attribute("type", "checkbox")
                                    isVis |> AVal.map (fun v ->
                                        if v then Some (Attribute("checked", "checked")) else None
                                    )
                                    Dom.OnClick(fun _ ->
                                        let current = AVal.force isVis
                                        env.Emit [SetVisible(name, not current)]
                                    )
                                }
                                " " + name
                            }
                        }
                    )

                    ul {
                        li { "ctrl+click or long-press to filter mesh" }
                        li { "double-click to focus camera" }
                    }
                }

                // ── Tab 2: Info ───────────────────────────────────
                div {
                    Class "tab-panel"
                    Attribute("id", "hud-panel2")
                    h1 { "Info" }
                    p { "Aardvark.Dom WebAssembly scaffold." }
                    p { "Elm-style architecture: Model / Update / View." }
                    p { "Rendering via WebGL with an orbit camera." }
                }

                // ── Tab 3: Settings ───────────────────────────────
                div {
                    Class "tab-panel"
                    Attribute("id", "hud-panel3")
                    h1 { "Settings" }
                    p { "Nothing to configure yet." }
                }
            }
        }

    let debugLog (model : AdaptiveModel) =
        div {
            Style [
                Position "fixed"; Bottom "0"; Left "0"; Right "0"
                MaxHeight "30vh"; OverflowY "auto"
                Background "rgba(0,0,0,0.8)"; Color "#0f0"
                FontFamily "monospace"; FontSize "11px"
                Padding "4px 8px"; PointerEvents "none"
                ZIndex 9999; StyleProperty("white-space", "pre-wrap")
            ]
            model.DebugLog |> AList.map (fun line -> div { line })
        }

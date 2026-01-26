open Aardvark.Base
open Aardworx.WebAssembly.Dom
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Utilities
open Aardworx.Rendering.WebGL
open Aardworx.WebAssembly


[<EntryPoint>]
let main args =
    task {
        do! Window.Document.Ready
        
        let app = new WebGLApplication(CommandStreamMode.Managed)
        
        Boot.runView app (fun ctx ->
            body {
                Style [ Background "white" ]
                h1 { "Hi Atti" }
               
                renderControl {
                    SimpleFreeFlyController {
                        Location = V3d.III * 6.0
                        LookAt = V3d.Zero
                        Sky = V3d.OOI
                        Config = None
                        AnimationFinished = None
                    }
                    
                    let! info = RenderControl.Info
                    
                    Sg.Proj (info.ViewportSize |> AVal.map (fun s -> Frustum.perspective 120.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo))
                    
                    let mutable first = true
                    RenderControl.OnRendered (fun _ ->
                        if first then
                            let el = Window.Document.GetElementById("loader")
                            if not (isNull el) then el.Remove()
                            first <- false
                    )
                    
                    Sg.Shader {
                        DefaultSurfaces.trafo
                        DefaultSurfaces.simpleLighting
                    }
                    
                    Primitives.Box(V3d.III, C4b.Red)
                    
                }
            }    
        )
        
        
        
    } |> ignore
    0
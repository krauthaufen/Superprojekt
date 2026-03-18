namespace Superprojekt

open FSharp.Data.Adaptive
open Aardvark.Base
open Adaptify
open Aardvark.Dom


[<ModelType>]
type Model =
    {
        Camera         : OrbitState
        Value          : int
        Hover          : option<V3d>
        MeshNames      : IndexList<string>       // ordered; drives render loop
        MeshVisible    : Map<string, bool>        // aval<Map> — visibility per name
        CommonCentroid : V3d                      // reference origin for rendering
        MenuOpen       : bool
        
        [<CheapEquals>]
        Filtered       : HashMap<string, int[]>
        FilterCenter   : option<V3d>
        DebugLog       : IndexList<string>
        
        CurrentHoverPosition : Option<V3d>
    }

module Model =
    let initial =
        {
            Value          = 3
            Hover          = None
            Camera         = OrbitState.create V3d.Zero 1.0 0.3 3.0 Button.Left Button.Middle
            MeshNames      = IndexList.empty
            MeshVisible    = Map.empty
            CommonCentroid = V3d.Zero
            MenuOpen       = false
            Filtered        = HashMap.empty
            FilterCenter    = None
            DebugLog       = IndexList.empty
            CurrentHoverPosition = None
        }

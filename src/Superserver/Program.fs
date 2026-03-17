open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Aardvark.Base
open Aardvark.Data.Wavefront

module MeshLoader =

    let private findDataRoot () =
        let mutable dir = AppContext.BaseDirectory
        let mutable result = None
        while result.IsNone && not (isNull dir) do
            let candidate = Path.Combine(dir, "data")
            if Directory.Exists candidate then result <- Some candidate
            else dir <- Path.GetDirectoryName dir
        result |> Option.defaultWith (fun () -> failwith "data folder not found")

    let dataRoot = lazy findDataRoot ()

    let private objFiles (folder : string) =
        Directory.GetFiles(folder, "*.obj") |> Array.sort

    let meshCount (name : string) =
        let folder = Path.Combine(dataRoot.Value, name)
        if Directory.Exists folder then objFiles folder |> Array.length else 0

    // Binary layout (little-endian):
    //   magic           4 bytes  "MESH"
    //   vertexCount     int32
    //   indexCount      int32
    //   centroid X/Y/Z  3 × float64
    //   positions       vertexCount × 3 × float32   (centroid-relative)
    //   uvs             vertexCount × 2 × float32
    //   indices         indexCount × int32

    let serialize (name : string) (index : int) : byte[] =
        let folder = Path.Combine(dataRoot.Value, name)
        if not (Directory.Exists folder) then failwithf "not found: %s" name

        let files = objFiles folder
        if index < 0 || index >= files.Length then
            failwithf "mesh index %d out of range (folder has %d)" index files.Length

        let objFile  = files.[index]
        let baseName = Path.GetFileNameWithoutExtension objFile
        let jpgFile  = Path.Combine(folder, baseName + "_atlas.jpg")
        if not (File.Exists jpgFile) then failwithf "atlas not found: %s" jpgFile

        let centroidFile =
            Directory.GetFiles(folder, "*_centroid.txt") |> Array.tryHead
            |> Option.defaultWith (fun () -> failwithf "no centroid.txt in %s" folder)

        let centroid =
            let parts = File.ReadAllText(centroidFile).Trim().Split(' ')
            V3d(float parts.[0], float parts.[1], float parts.[2])

        let mesh = ObjParser.Load objFile

        let positions =
            match mesh.Vertices with
            | :? System.Collections.Generic.IList<V3f> as v -> v |> Seq.map V3d |> Seq.toArray
            | :? System.Collections.Generic.IList<V3d> as v -> v |> Seq.toArray
            | :? System.Collections.Generic.IList<V4f> as v -> v |> Seq.map (fun p -> V3d(float p.X, float p.Y, float p.Z)) |> Seq.toArray
            | :? System.Collections.Generic.IList<V4d> as v -> v |> Seq.map (fun p -> V3d(p.X, p.Y, p.Z)) |> Seq.toArray
            | _ -> [||]

        let texCoords =
            if isNull mesh.TextureCoordinates then [||]
            else mesh.TextureCoordinates |> Seq.map Vec.xy |> Seq.toArray

        let vertexMap = System.Collections.Generic.Dictionary<struct(int * int), int>()
        let outPos = ResizeArray<V3f>()
        let outUv  = ResizeArray<V2f>()
        let outIdx = ResizeArray<int>()

        for set in mesh.FaceSets do
            let iPos = set.VertexIndices
            let iTc  = if isNull set.TexCoordIndices then iPos else set.TexCoordIndices
            for ti in 0 .. set.ElementCount - 1 do
                let fi  = set.FirstIndices.[ti]
                let cnt = set.FirstIndices.[ti + 1] - fi
                if cnt = 3 then
                    let p0 = positions.[iPos.[fi]]
                    let p1 = positions.[iPos.[fi + 1]]
                    let p2 = positions.[iPos.[fi + 2]]
                    if not (Vec.AnyNaN p0 || Vec.AnyNaN p1 || Vec.AnyNaN p2) then
                        for k in 0 .. 2 do
                            let pi  = iPos.[fi + k]
                            let uvi = iTc.[fi + k]
                            let key = struct(pi, uvi)
                            let idx =
                                match vertexMap.TryGetValue key with
                                | true, i -> i
                                | _ ->
                                    let i = outPos.Count
                                    outPos.Add(V3f positions.[pi])
                                    outUv.Add(if uvi < texCoords.Length then texCoords.[uvi] else V2f.Zero)
                                    vertexMap.[key] <- i
                                    i
                            outIdx.Add idx

        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)
        bw.Write("MESH"B)
        bw.Write(outPos.Count)
        bw.Write(outIdx.Count)
        bw.Write(centroid.X)
        bw.Write(centroid.Y)
        bw.Write(centroid.Z)
        for p in outPos do
            bw.Write(p.X)
            bw.Write(p.Y)
            bw.Write(p.Z)
        for uv in outUv do
            bw.Write(uv.X)
            bw.Write(uv.Y)
        for i in outIdx do
            bw.Write(i)
        ms.ToArray()

    let atlasPath (name : string) (index : int) =
        let folder = Path.Combine(dataRoot.Value, name)
        let files  = objFiles folder
        if index < 0 || index >= files.Length then None
        else
            let baseName = Path.GetFileNameWithoutExtension files.[index]
            let jpg = Path.Combine(folder, baseName + "_atlas.jpg")
            if File.Exists jpg then Some jpg else None


// GET /centroids         → { "name": [x,y,z], ... }
// GET /mesh/{name}       → mesh count (int, plain text)
// GET /mesh/{name}/{i}   → binary mesh data
// GET /mesh/{name}/{i}/atlas → atlas JPEG

let centroidsHandler : HttpHandler =
    fun next ctx -> task {
        let result = System.Collections.Generic.Dictionary<string, float[]>()
        for dir in Directory.GetDirectories(MeshLoader.dataRoot.Value) |> Array.sort do
            let name = Path.GetFileName dir
            match Directory.GetFiles(dir, "*_centroid.txt") |> Array.tryHead with
            | Some f ->
                let parts = File.ReadAllText(f).Trim().Split(' ')
                result.[name] <- [| float parts.[0]; float parts.[1]; float parts.[2] |]
            | None -> ()
        return! json result next ctx
    }

let meshCountHandler (name : string) : HttpHandler =
    fun next ctx -> task {
        let count = MeshLoader.meshCount name
        if count = 0 then
            return! RequestErrors.notFound (text $"not found: {name}") next ctx
        else
            return! text (string count) next ctx
    }

let meshHandler (name : string, index : int) : HttpHandler =
    fun next ctx -> task {
        try
            let bytes = MeshLoader.serialize name index
            ctx.Response.ContentType <- "application/octet-stream"
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            return! next ctx
        with ex ->
            return! RequestErrors.notFound (text ex.Message) next ctx
    }

let atlasHandler (name : string, index : int) : HttpHandler =
    fun next ctx -> task {
        match MeshLoader.atlasPath name index with
        | None ->
            return! RequestErrors.notFound (text $"atlas not found: {name}/{index}") next ctx
        | Some path ->
            ctx.Response.ContentType <- "image/jpeg"
            let bytes = File.ReadAllBytes path
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            return! next ctx
    }

let webApp =
    choose [
        route "/"                       >=> text "hello"
        route "/centroids"              >=> centroidsHandler
        routef "/mesh/%s/%i/atlas"      atlasHandler
        routef "/mesh/%s/%i"            meshHandler
        routef "/mesh/%s"               meshCountHandler
    ]

[<EntryPoint>]
let main args =
    Aardvark.Init()
    let builder = WebApplication.CreateBuilder(args)
    builder.Services.AddCors() |> ignore
    builder.Services.AddGiraffe() |> ignore
    let app = builder.Build()
    app.UseCors(fun p -> p.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader() |> ignore)
    app.UseGiraffe(webApp)
    app.Run()
    0

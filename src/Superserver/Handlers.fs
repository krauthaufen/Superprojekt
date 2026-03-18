module Handlers

open System
open System.IO
open Microsoft.AspNetCore.Http
open Giraffe
open Aardvark.Base
open Aardvark.Embree

// ── JSON request types ────────────────────────────────────────────────────────

// All coordinates in absolute world space (double precision).
// Server converts: localPos = V3f(worldPos - mesh.centroid)

[<CLIMutable>]
type RayRequest     = { Name: string; Index: int; Origin: float[]; Direction: float[] }

[<CLIMutable>]
type ClosestRequest = { Name: string; Index: int; Point: float[] }

[<CLIMutable>]
type SphereRequest  = { Name: string; Index: int; Center: float[]; Radius: float }

[<CLIMutable>]
type BoxRequest     = { Name: string; Index: int; Min: float[]; Max: float[] }

let inline private toV3d (a : float[]) = V3d(a.[0], a.[1], a.[2])
let inline private fromV3d (v : V3d)   = [| v.X; v.Y; v.Z |]

// ── HTTP handlers ─────────────────────────────────────────────────────────────

// GET /api/centroids
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

// GET /api/mesh/{name}
let meshCountHandler (name : string) : HttpHandler =
    fun next ctx -> task {
        let count = MeshLoader.meshCount name
        if count = 0 then return! RequestErrors.notFound (text $"not found: {name}") next ctx
        else            return! text (string count) next ctx
    }

// GET /api/mesh/{name}/{i}
let meshHandler (name : string, index : int) : HttpHandler =
    fun next ctx -> task {
        try
            // Load through cache so Embree scene + BVH are ready for queries
            let lm     = MeshCache.get name index
            let pm     = lm.parsed
            let folder = Path.Combine(MeshLoader.dataRoot.Value, name)
            let files  = Directory.GetFiles(folder, "*.obj") |> Array.sort
            let base'  = Path.GetFileNameWithoutExtension files.[index]
            let jpg    = Path.Combine(folder, base' + "_atlas.jpg")
            if not (File.Exists jpg) then failwithf "atlas not found: %s" jpg
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            bw.Write("MESH"B)
            bw.Write(pm.positions.Length)
            bw.Write(pm.indices.Length)
            bw.Write(pm.centroid.X); bw.Write(pm.centroid.Y); bw.Write(pm.centroid.Z)
            for p  in pm.positions do bw.Write(p.X);  bw.Write(p.Y);  bw.Write(p.Z)
            for uv in pm.uvs       do bw.Write(uv.X); bw.Write(uv.Y)
            for i  in pm.indices   do bw.Write(i)
            let bytes = ms.ToArray()
            ctx.Response.ContentType <- "application/octet-stream"
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            return! next ctx
        with ex -> return! RequestErrors.notFound (text ex.Message) next ctx
    }

// GET /api/mesh/{name}/{i}/atlas
let atlasHandler (name : string, index : int) : HttpHandler =
    fun next ctx -> task {
        match MeshLoader.atlasPath name index with
        | None -> return! RequestErrors.notFound (text $"atlas not found: {name}/{index}") next ctx
        | Some path ->
            ctx.Response.ContentType <- "image/jpeg"
            let bytes = File.ReadAllBytes path
            do! ctx.Response.Body.WriteAsync(bytes, 0, bytes.Length)
            return! next ctx
    }

// POST /api/query/ray
// body: { name, index, origin:[x,y,z], direction:[x,y,z] }
// resp: { hit, t, point:[x,y,z], triangleId }  or  { hit:false }
let rayHandler : HttpHandler =
    fun next ctx -> task {
        let! req = ctx.BindJsonAsync<RayRequest>()
        let lm   = MeshCache.get req.Name req.Index
        let c    = lm.parsed.centroid
        let orig = V3f(toV3d req.Origin - c)
        let dir  = V3f(toV3d req.Direction)
        let mutable hit = RayHit()
        let ok = lm.scene.Intersect(orig, dir, &hit)
        if ok then
            let localHit = orig + dir * hit.T
            let worldHit = V3d localHit + c
            return! json {| hit = true; t = hit.T; point = fromV3d worldHit; triangleId = int hit.PrimitiveId |} next ctx
        else
            return! json {| hit = false |} next ctx
    }

// POST /api/query/closest
// body: { name, index, point:[x,y,z] }
// resp: { found, point:[x,y,z], distanceSquared, triangleId }  or  { found:false }
let closestHandler : HttpHandler =
    fun next ctx -> task {
        let! req = ctx.BindJsonAsync<ClosestRequest>()
        let lm   = MeshCache.get req.Name req.Index
        let c    = lm.parsed.centroid
        let qp   = V3f(toV3d req.Point - c)
        let res  = lm.scene.GetClosestPoint(qp)
        if res.IsValid then
            let worldPt = V3d res.Point + c
            return! json {| found = true; point = fromV3d worldPt; distanceSquared = res.DistanceSquared; triangleId = int res.PrimID |} next ctx
        else
            return! json {| found = false |} next ctx
    }

let private binaryIndices (tris : int[]) : HttpHandler =
    fun next ctx -> task {
        ctx.SetContentType "application/octet-stream"
        let len = tris.Length
        let buf = Array.zeroCreate<byte> (4 + len * 4)
        System.BitConverter.TryWriteBytes(buf.AsSpan(0, 4), len) |> ignore
        System.Buffer.BlockCopy(tris, 0, buf, 4, len * 4)
        ctx.Response.ContentLength <- Nullable<int64>(int64 buf.Length)
        do! ctx.Response.Body.WriteAsync(buf, 0, buf.Length)
        return Some ctx
    }

// POST /api/query/sphere
// body: { name, index, center:[x,y,z], radius }
// resp: binary  int32 count | int32[] indices
let sphereHandler : HttpHandler =
    fun next ctx -> task {
        let! req = ctx.BindJsonAsync<SphereRequest>()
        let lm   = MeshCache.get req.Name req.Index
        let c    = lm.parsed.centroid
        let lc   = V3f(toV3d req.Center - c)
        let tris = MeshCache.trianglesInSphere lm lc (float32 req.Radius)
        return! binaryIndices tris next ctx
    }

// POST /api/query/box
// body: { name, index, min:[x,y,z], max:[x,y,z] }
// resp: binary  int32 count | int32[] indices
let boxHandler : HttpHandler =
    fun next ctx -> task {
        let! req = ctx.BindJsonAsync<BoxRequest>()
        let lm   = MeshCache.get req.Name req.Index
        let c    = lm.parsed.centroid
        let lMin = V3f(toV3d req.Min - c)
        let lMax = V3f(toV3d req.Max - c)
        let tris = MeshCache.trianglesInBox lm lMin lMax
        return! binaryIndices tris next ctx
    }

// ── Routing ───────────────────────────────────────────────────────────────────

let webApp : HttpHandler =
    choose [
        route "/api/centroids"              >=> centroidsHandler
        routef "/api/mesh/%s/%i/atlas"      atlasHandler
        routef "/api/mesh/%s/%i"            meshHandler
        routef "/api/mesh/%s"               meshCountHandler
        route "/api/query/ray"              >=> rayHandler
        route "/api/query/closest"          >=> closestHandler
        route "/api/query/sphere"           >=> sphereHandler
        route "/api/query/box"              >=> boxHandler
    ]

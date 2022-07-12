open System
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

let rootDir = __SOURCE_DIRECTORY__  </> ".."

let sln = rootDir </> "Plough.ControlFlow.sln"

let srcGlob = rootDir </> "src/**/*.??proj"
let testsGlob = rootDir </> "test/**/*.??proj"


let distDir = rootDir </> "dist"
let distGlob = distDir </> "*.nupkg"

// Skip the first arg since it will be the path of the binary running
Environment.GetCommandLineArgs()[1..]
|> Array.toList
|> Context.FakeExecutionContext.Create false "build.fsx"
|> Context.RuntimeContext.Fake
|> Context.setExecutionContext

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    [distDir]
    |> Shell.cleanDirs

    !! srcGlob
    ++ testsGlob
    |> Seq.collect(fun p ->
        ["bin";"obj"]
        |> Seq.map(fun sp -> IO.Path.GetDirectoryName p </> sp ))
    |> Shell.cleanDirs

    [
        rootDir </> "paket-files/paket.restore.cached"
    ]
    |> Seq.iter Shell.rm
)

Target.create "Restore" (fun _ ->
    [sln]
    |> Seq.iter (DotNet.restore id)
)

Target.create "Build" (fun _ ->
    [sln]
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
  [sln]
  |> Seq.iter (DotNet.test id)
)

Target.create "Pack" (fun _ ->
    let release = ReleaseNotes.load "RELEASE_NOTES.md"

    Paket.pack(fun p ->
        { p with
            ToolType = ToolType.CreateCLIToolReference()
            BuildConfig = "Release"
            OutputPath = "bin"
            MinimumFromLockFile = true
            IncludeReferencedProjects = true
            Version = release.AssemblyVersion
            TemplateFile = "paket.template"
            ReleaseNotes = String.toLines release.Notes })  

)

"Build" ==> "Pack"

Target.create "Publish" (fun _ ->
    // target not yet working
    // dotnet nuget push Plough.ControlFlow.<version>.nupkg -as https://api.nuget.org/v3/index.json -k <api key>
    Paket.push(fun p ->
        { p with
            WorkingDir = "bin" })
)

Target.create "All" ignore

"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefaultWithArguments  "All"

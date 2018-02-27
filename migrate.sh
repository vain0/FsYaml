set -eu
dotnet new -i Persimmon.Templates::*

rm FsYaml.sln
rm src/FsYaml/*.fsproj
rm src/FsYaml/paket.*
rm tests/FsYaml.Tests/*.fsproj
rm tests/FsYaml.Tests/paket.*
rm paket.*
rm -rf .paket/

dotnet new sln -n FsYaml
cd src
dotnet new classlib -lang F# -o FsYaml
cd ../tests
dotnet new persimmon -o FsYaml.Tests
cd ..
dotnet sln add ./src/FsYaml/FsYaml.fsproj
dotnet sln add ./tests/FsYaml.Tests/FsYaml.Tests.fsproj
dotnet add ./tests/FsYaml.Tests/FsYaml.Tests.fsproj reference ./src/FsYaml/FsYaml.fsproj
dotnet add ./src/FsYaml/FsYaml.fsproj package YamlDotNet
dotnet add ./tests/FsYaml.Tests/FsYaml.Tests.fsproj package FSharp.Compiler.Service
dotnet build

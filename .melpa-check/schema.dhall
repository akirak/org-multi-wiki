let PackageName : Type = Text
let File : Type = Text
let Recipe : Type = Text
let Version : Type = Text
in
{
  Package = {
    Type = {
      pname : PackageName,
      version : Version,
      files : List File,
      localDependencies : List PackageName,
      dependencies : List PackageName,
      mainFile : Optional File,
      recipe : Recipe
    },
    default = {
      localDependencies = [] : List File,
      mainFile = None File
    }
  }
}

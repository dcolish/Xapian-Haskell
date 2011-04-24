> import Distribution.Simple
> import Distribution.Simple.Setup
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription

> import Control.Arrow ((***))
> import Control.Monad (forM_)
> import Data.Maybe (fromMaybe)
> import qualified Data.List as List
> import System.FilePath (joinPath, replaceExtension, takeBaseName)
> import System.Directory (createDirectoryIfMissing)
> import System (system)

For Xapian-Haskell we need to compile the C bindings to the Xapian C++ library
with a C++ compiler. This is done via a procedure that will is hooked into the
building sequence as 'buildHook' hook. We use simpleUserHooks as basis.

> main :: IO ()
> main = defaultMainWithHooks simpleUserHooks{ buildHook = cplusplusHook }

Now we go on defining the cplusplusHook.

> cplusplusHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
> cplusplusHook desc bInfo hooks bFlags =

First extract the list of C++ sources, compiler and compiler options from the
package description 'desc'

>  do let ( cc_sources
>           , cc_compiler
>           , cc_options ) = fromMaybe ([], "/usr/bin/c++", "-Wall -c") $
>          do lib <- library desc
>             let fields = customFieldsBI $ libBuildInfo lib
>             src <- fmap words $ List.lookup "x-cc-sources" fields
>             cmp <- List.lookup "x-cc-compiler" fields
>             opt <- List.lookup "x-cc-options" fields
>             return (src, cmp, opt)

Get the build directory or create it

>     let builddir = buildDir bInfo
>     createDirectoryIfMissing True builddir

Compile each source and put the resulting object files into the build dir

>     let object_of source = joinPath [builddir, replaceExtension (takeBaseName source) "o"]
>
>     forM_ cc_sources $ \source ->
>      do let command = unwords $ [ cc_compiler, cc_options, source, "-o"
>                                 , object_of source]
>         putStrLn command
>         system command

Pass the list of object files to the parameters for the Haskell compiler.

>     let objects = map object_of cc_sources
>     let lib' =
>          do lib <- library desc
>             let bInfo'   = libBuildInfo lib
>                 options' = options bInfo'
>                 bInfo''  = bInfo' {options = map (id *** (++ objects)) options'}
>             return $ lib{ libBuildInfo = bInfo'' }
>     let desc' = desc { library = lib' }

Since we now prepared all the object files, we can proceed with the normal
building.

>     buildHook simpleUserHooks desc' bInfo hooks bFlags
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Development.Shake
import Control.Monad
import Data.List
import Development.Shake.Command
import Data.Foldable
import Development.Shake.FilePath
import System.FilePath
import Development.Shake.Util
import Data.Semigroup
import Data.Typeable
import Data.Hashable
import Control.DeepSeq
import Data.Binary

build = "build"
src = "src"
includeDir = src
top = "Zoid.v"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = build } $ do
  want [build </> top]

  phony "clean" $ do
    putNormal $ "Cleaning files in " <> show build
    removeFilesAfter build ["//*"]

  "//*.v" %> \out -> do
    putNormal out
    clash out Verilog

-- | Find all the hand written Verilog files as well as the verilog files
-- compiled with Clash.
gatherVerilog :: Action [FilePath]
gatherVerilog = do
  nativeVerilogSources <- getDirectoryFiles "" [src <> "//*.v"]
  haskellSources       <- getDirectoryFiles "" [src <> "//*.hs"]
  let haskellBuild = dropExtensions . srcToBuild src build <$> haskellSources
      listingFiles = flip replaceExtension "listing" <$> haskellBuild
  -- need listingFiles
  haskellHDLFiles <- fmap read <$> traverse readFile' listingFiles
  mapM_ putNormal haskellHDLFiles
  pure $ nativeVerilogSources -- <> concat haskellHDLFiles

replaceBaseDir :: FilePath -> FilePath -> FilePath
replaceBaseDir path rep =
  let dirs = splitDirectories path
   in go dirs
  where go (base:other:rest) = joinPath (rep:other:rest)
        go _ = path

srcToBuild :: FilePath -> FilePath -> FilePath -> FilePath
srcToBuild source build path =
  let dirs = splitDirectories path
      replaceEqual cond rep el = if el == cond then rep else el 
   in joinPath (replaceEqual source build <$> dirs)
  
-- icestormRules :: Rules ()
-- icestormRules = do
--   -- Use Yosys to compile some Verilog to a BLIF file
--   build </> "*.blif" %> \out -> do
--     vs <- gatherVerilog
--     needed vs
--     cmd_ "yosys -q -p" ["synth_ice40 -blif " <> out] vs

--   build </> "*.txt" %> \out -> do
--     let blif = out -<.> "blif"
--     need [blif]
--     cmd_ "arachne-pnr -d 8k -P tq144:4k -p" pcfFile blif "-o" out

--   build </> "*.bin" %> \out -> do
--     let txt = out -<.> "txt"
--     need [txt]
--     cmd_ "icepack" txt out

clashRules :: Rules ()
clashRules = do
  build </> "*.dep" %> \out -> do
    let bn      = takeBaseName out
        hs      = src </> bn <.> "hs"
        include = "-i" <> includeDir
    cmd_ "clash" include "-M" "-dep-suffix" [""] "-dep-makefile" out hs

  -- Writes a list of HDL files, Clash can compile several files
  -- at once, amortizing the overhead of reading primitives
  batch
    maxBound
    ((build </> "*.listing") %>)
    (\out -> do
      let bn  = takeBaseName out
          dep = build </> bn <.> "dep"
      needSomeMakefileDepends (".hs" `isSuffixOf`) dep
      pure out
    )
    (\outs -> do
      let bns = takeBaseName <$> outs
      vss <- compileWithClash bns
      zipWithM_ writeFile' outs (show <$> vss)
    )

data Language
  = Verilog
  | Vhdl
  | SystemVerilog

instance Show Language where
  show Verilog = "verilog"
  show Vhdl = "vhdl"
  show SystemVerilog = "systemverilog"

clash :: FilePath -> Language -> Action ()
clash path lang =
  let hss     = replaceBaseDir path src -<.> "hs"
      include = "-i" <> includeDir
      hdl     = show lang
   in cmd_ "clash" include "-odir" build "-outputdir" build ("--" <> hdl) hss

-- | Given the name of some Haskell modules, compile them with Clash and write
-- the output Verilog files to a ".listing" file in the build directory.
--
-- TODO: Ignore testbench hdl files
compileWithClash :: [String] -> Action [[FilePath]]
compileWithClash basenames = do
  let hss     = (\b -> src </> b <.> "hs") <$> basenames
      include = "-i" <> includeDir
      hdl     = "verilog"
  cmd_ "clash" include "-odir" build "-outputdir" build ("--" <> hdl) hss
  vs <- traverse (\b -> getDirectoryFiles "" [build </> hdl </> b <> "//*.v"])
                 basenames
  pure vs

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Depend on some specific inputs in a makefile
needSomeMakefileDepends :: (FilePath -> Bool) -> FilePath -> Action ()
needSomeMakefileDepends p makefile = do
  contents <- readFile' makefile
  let deps = snd =<< parseMakefile contents
  needed (filter p deps)

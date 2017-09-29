{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid
import           Data.Prototype
import           Lens.Micro.Platform              ((.=))
import           Yi.Boot                          (configMain, reload)
import           Yi.Config
import           Yi.Config.Default                (defaultConfig)
import           Yi.Config.Default.HaskellMode    (configureHaskellMode)
import           Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import           Yi.Config.Default.MiscModes      (configureMiscModes)
import           Yi.Config.Default.Vim
import           Yi.Config.Default.Pango
import           Yi.Config.Simple hiding          (super)
import qualified Yi.Keymap.Vim as V
import qualified Yi.Keymap.Vim.Common as V
import qualified Yi.Keymap.Vim.Ex.Types as V
import qualified Yi.Keymap.Vim.Ex.Commands.Common as V
import qualified Yi.Keymap.Vim.Utils as V
import qualified Yi.Mode.Haskell as Haskell
import qualified Yi.Rope as R
import Numeric (readHex)
import qualified Data.List as List

main :: IO ()
main = configMain defaultConfig $ do
         fontSize .= Just 12
         cursorStyle .= AlwaysFat
         theme .= myTheme Light
         configurePango
         myVimConfig
         configureHaskellMode
         configureJavaScriptMode
         configureMiscModes

data Background = Light | Dark

myTheme :: Background -> Theme
myTheme background = 
    let colorFromHex :: String -> Color
        colorFromHex hex = let splitHex hex = let (begin, end) = List.splitAt 4 hex
                                                  (start, middle) = List.splitAt 2 begin
                                              in map (\hex -> let [(int,_)] = readHex hex in int) [start, middle, end]
                               colorFromList (r:g:b:_) = RGB r g b
                           in colorFromList $ splitHex hex
        yellow  = colorFromHex "b58900"
        orange  = colorFromHex "cb4b16"
        red     = colorFromHex "dc322f"
        magenta = colorFromHex "d33682"
        violet  = colorFromHex "6c71c4"
        blue    = colorFromHex "268bd2"
        cyan    = colorFromHex "2aa198"
        green   = colorFromHex "859900"
        base03  = colorFromHex "002b36"
        base02  = colorFromHex "073642"
        base01  = colorFromHex "586e75"
        base00  = colorFromHex "657b83"
        base0   = colorFromHex "839496"
        base1   = colorFromHex "93a1a1"
        base2   = colorFromHex "eee8d5"
        base3   = colorFromHex "fdf6e3"
        bgColor = case background of Light -> base3
                                     Dark  -> base03
        fgColor = case background of Light -> base00
                                     Dark  -> base0
        commentColor = case background of Light -> base1
                                          Dark  -> base01
        selectionColor = case background of Light -> base2
                                            Dark  -> base02
    in defaultTheme `override` \super _ -> super
        { baseAttributes = Attributes { foreground = fgColor
                                      , background = bgColor
                                      , reverseAttr = False
                                      , bold = False
                                      , italic = False
                                      , underline = False
                                      }
        , modelineAttributes   = emptyAttributes { foreground = fgColor, background = bgColor }
        , modelineFocusStyle   = withFg base2
        , tabNotFocusedStyle   = mempty
        , selectedStyle        = withBg selectionColor
        , hintStyle            = withFg cyan `mappend` withBg base02
        , strongHintStyle      = withFg magenta `mappend` withBg base02
        , commentStyle         = withFg commentColor
        , blockCommentStyle    = withFg commentColor
        , eofStyle             = withFg blue
        , errorStyle           = withFg red
        , keywordStyle         = withFg blue
        , numberStyle          = withFg red
        , preprocessorStyle    = withFg orange
        , stringStyle          = withFg green
        , longStringStyle      = withFg green
        , typeStyle            = withFg blue
        , dataConstructorStyle = withFg green
        , importStyle          = withFg blue
        , builtinStyle         = withFg blue
        , regexStyle           = withFg red
        , variableStyle        = withFg base0
        , operatorStyle        = withFg violet
        , makeFileRuleHead     = withFg blue
        , makeFileAction       = withFg base1
        , quoteStyle           = withFg base1
        }

myVimConfig :: ConfigM ()
myVimConfig = do
  configureVim
  defaultKmA .= myKeymapSet
  modeTableA .= myModes
  configCheckExternalChangesObsessivelyA .= False

myKeymapSet :: KeymapSet
myKeymapSet = V.mkKeymapSet $ V.defVimConfig `override` \super this ->
    let eval = V.pureEval this
    in super
        { -- Here we can add custom bindings.
          -- See Yi.Keymap.Vim.Common for datatypes and
          -- Yi.Keymap.Vim.Utils for useful functions like mkStringBindingE

          -- In case of conflict, that is if there exist multiple bindings
          -- whose prereq function returns WholeMatch,
          -- the first such binding is used.
          -- So it's important to have custom bindings first.
          V.vimBindings = myBindings eval <> V.vimBindings super
        , V.vimRelayout = workmanRelayout
        , V.vimExCommandParsers = exReload : V.vimExCommandParsers super
        }

workmanRelayout :: Char -> Char
workmanRelayout = V.relayoutFromTo workmanLayout qwertyLayout
    where workmanLayout = concat ["qdrwbjfup;[]", "ashtgyneoi'", "zxmcvkl" ]
          qwertyLayout  = concat ["qwertyuiop[]", "asdfghjkl;'", "zxcvbnm" ]

myBindings :: (V.EventString -> EditorM ()) -> [V.VimBinding]
myBindings eval =
    let nmap x y = V.mkStringBindingE V.Normal V.Drop (x, y, id)
        imap x y = V.VimBindingE (\evs state -> case V.vsMode state of
                                    V.Insert _ ->
                                        fmap (const (y >> return V.Continue))
                                             (evs `V.matchesString` x)
                                    _ -> V.NoMatch)
    in [ nmap "<C-h>" previousTabE
       , nmap "<C-l>" nextTabE

         -- Press space to clear incremental search highlight
       , nmap " " (eval ":nohlsearch<CR>")

       , nmap "<F3>" (withCurrentBuffer deleteTrailingSpaceB)
       , nmap "<F4>" (withCurrentBuffer moveToSol)
       , nmap "<F1>" (withCurrentBuffer readCurrentWordB >>= printMsg . R.toText)
       , nmap "<C-s>" (eval ":write<CR>")
       , imap "<Home>" (withCurrentBuffer moveToSol)
       , imap "<End>" (withCurrentBuffer moveToEol)
       ]

myModes :: [AnyMode]
myModes =
    [ AnyMode Haskell.fastMode
      { -- Disable beautification
          modePrettify = const $ return ()
      }
    ]

exReload :: V.EventString -> Maybe V.ExCommand
exReload "reload" = Just $ V.impureExCommand
  { V.cmdShow = "reload"
  , V.cmdAction = YiA reload
  }
exReload _ = Nothing

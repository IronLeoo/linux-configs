import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , terminal = "kitty"
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep		= magenta " | "
    , ppTitleSanitize	= xmobarStrip
    , ppCurrent		= wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden		= white . wrap " " ""
    , ppHiddenNoWindows	= lowWhite . wrap " " ""
    , ppUrgent		= red . wrap (yellow "!") (yellow "!")
    , ppOrder		= \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras		= [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused	= wrap (white "[") (white "]") . magenta .ppWindow
    formatUnfocused	= wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue,  lowWhite, magenta, red, white, yellow :: String -> String
    magenta	= xmobarColor "#ff79c6" ""
    blue	= xmobarColor "#bd93f9" ""
    white	= xmobarColor "#f8f8f2" ""
    yellow	= xmobarColor "#f1fa8c" ""
    red		= xmobarColor "#ff5555" ""
    lowWhite	= xmobarColor "#bbbbbb" ""

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myStartupHook = do
	spawnOnce "nitrogen --restore &"
	spawnOnce "compton &"

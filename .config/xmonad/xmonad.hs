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
    , normalBorderColor = "#ffffff"
    , focusedBorderColor = "#feaadb"
    , borderWidth = 3
    , focusFollowsMouse = False
    }

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    ]

myXmobarPP :: PP
myXmobarPP = def
    { ppSep		= purple " | "
    , ppTitleSanitize	= xmobarStrip
    , ppCurrent		= pink . wrap " " "" . xmobarBorder "Top" "#feaadb" 2
    , ppHidden		= purple . wrap " " ""
    , ppHiddenNoWindows	= lowPurple . wrap " " ""
    , ppUrgent		= red . wrap (yellow "!") (yellow "!")
    , ppOrder		= \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras		= [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused	= wrap (white "[") (white "]") . pink . ppWindow
    formatUnfocused	= wrap (purple "[") (purple "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue,  lowWhite, magenta, red, white, yellow, purple, lowPurple, pink :: String -> String
    magenta	= xmobarColor "#ff79c6" ""
    blue	= xmobarColor "#bd93f9" ""
    white	= xmobarColor "#f8f8f2" ""
    yellow	= xmobarColor "#f1fa8c" ""
    red		= xmobarColor "#ff5555" ""
    lowWhite	= xmobarColor "#bbbbbb" ""
    purple      = xmobarColor "#885393" ""
    lowPurple   = xmobarColor "#64366d" ""
    pink        = xmobarColor "#feaadb" ""

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myStartupHook = do
	spawnOnce "stalonetray --slot-size $BARHEIGHT --icon-size $ICONSIZE &"
        spawnOnce "cbatticon &"
        spawnOnce "volctl &"
        spawnOnce "nm-applet &"
	spawnOnce "nitrogen --restore &"
	spawnOnce "picom &"

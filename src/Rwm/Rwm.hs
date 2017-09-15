{-# LANGUAGE TemplateHaskell, FlexibleContexts, RecordWildCards
           , PatternSynonyms, ViewPatterns
           #-}
{-# OPTIONS_GHC -Wno-unused-matches
                -Wno-unused-local-binds
                -Wno-unused-top-binds
                -Wno-missing-signatures
                #-}

module Rwm.Rwm
  ( launch
  ) where

import Data.Bits ((.|.), zeroBits, (.&.), complement)
import Data.List (delete, subsequences)
import Data.Foldable (traverse_, for_, foldr')
import Control.Monad.RWS
import Control.Applicative
import Numeric (showHex)

import qualified System.Posix as Unix

import qualified Graphics.X11.Xlib        as X
import qualified Graphics.X11.Xlib.Extras as XE
import qualified Graphics.X11.Xrandr      as Xrr

import Control.Lens

-- Rwm monad
data Conn
  = Conn
    { _disp   ∷ X.Display
    , _screen ∷ X.ScreenNumber
    , _root   ∷ X.Window
    }
makeLenses ''Conn

data St
  = St
    { _wins ∷ [X.Window]
    }
makeLenses ''St

type Rwm a = RWST Conn () St IO a

runRwm conn rwm = runRWST rwm conn st0 where
  st0 = St []

quitRwm ∷ Rwm a
quitRwm = empty

fromI = fromIntegral

-- spawn
spawn ∷ String → IO Unix.ProcessID
spawn cmd = Unix.forkProcess (Unix.executeFile cmd True [] Nothing)

orphan ∷ String → IO ()
orphan cmd = do
  pid ← Unix.forkProcess (void $ spawn cmd)
  void $ Unix.getProcessStatus True False pid

-- hotkeys
type Hotkey = ((X.KeyMask, X.KeySym), Rwm ())
pattern Hotkey ∷ X.KeyMask → X.KeySym → Rwm () → Hotkey
pattern Hotkey m s a = ((m, s), a)

-- launch
launch ∷ IO ()
launch = do
  conn@(Conn disp scr root) ← connect

  X.allocaXEvent $ \pEv →
    void $ runRwm conn $ do
      configureInput
      configureDims

      addWindows =<< ourWindows

      orphan "termite" & io

      void $ many $ event pEv

  where
  io = liftIO

  connect = do
    disp ← X.openDisplay ""
    let scr = X.defaultScreen disp
    root ← X.rootWindow disp scr
    pure $ Conn disp scr root

  configureInput = do
    disp ← view disp; root ← view root
    X.selectInput disp root rootMask & io

    for_ hotKeys $ \(Hotkey mods sym _) → do
      kc ← X.keysymToKeycode disp sym & io
      for_ irrelStates $ \irr → do
        X.grabKey disp kc (irr .|. mods) root True gm gm & io

    X.sync disp False & io

    where
    gm = X.grabModeAsync

  irrelMods = [ X.lockMask ]
  irrelMask   = foldBitOr irrelMods
  irrelStates = foldBitOr <$> subsequences irrelMods
  foldBitOr = foldr' (.|.) zeroBits

  hotKeys ∷ [Hotkey]
  hotKeys =
    [ Hotkey X.noModMask    X.xK_u          (io $ putStrLn "poop")
    , Hotkey X.shiftMask    X.xK_u          (io $ putStrLn "worky")
    , Hotkey X.noModMask    X.xK_z          quitRwm
    ]

  rootMask = foldr1 (.|.) $
    [ X.substructureRedirectMask
    , X.substructureNotifyMask
    , X.enterWindowMask
    , X.keyPressMask
    , X.leaveWindowMask
    , X.structureNotifyMask
    , X.buttonPressMask
    ]

  configureDims = do
    disp ← view disp; root ← view root
    (Just scrCfg) ← Xrr.xrrGetScreenInfo disp root & io
    (rot, iSize) ← Xrr.xrrConfigCurrentConfiguration scrCfg & io
    (Just sizes) ← Xrr.xrrConfigSizes scrCfg & io
    let size = sizes !! (fromI iSize)
    pure ()

  addWindow w = wins %= (w:)
  addWindows = traverse_ addWindow

  releaseWindow w = wins %= (delete w)
  releaseWindows = traverse_ releaseWindow

  ourWindows = filterM should =<< allWindows where
    allWindows = do
      disp ← view disp; root ← view root
      (_, _, ws) ← XE.queryTree disp root & io
      pure ws

    should w = do
      disp ← view disp
      attrs ← XE.getWindowAttributes disp w & io
      let canRedir = not (XE.wa_override_redirect attrs)
          visible = XE.wa_map_state attrs == XE.waIsViewable
      pure (canRedir && visible)

  event pEv = do
    display ← view disp
    ev ← liftIO $ do
      X.nextEvent display pEv
      XE.getEvent pEv

    case ev of
      (XE.KeyEvent {..}) | ev_event_type == X.keyPress → do
        sym ← liftIO $ X.keycodeToKeysym display ev_keycode 0
        let modState' = ev_state .&. complement irrelMask
        case lookup (modState', sym) hotKeys of
          Just act → act
          _        → pure ()

      (XE.MapRequestEvent {..}) → do
        ws ← use wins
        when (not $ ev_window `elem` ws) $ do
          addWindow ev_window
          liftIO $ X.mapWindow display ev_window

      (XE.DestroyWindowEvent {..}) → do
        releaseWindow ev_window

      (XE.UnmapEvent {..}) → do
        releaseWindow ev_window

      (XE.MappingNotifyEvent {..}) → do
        X.refreshKeyboardMapping (ev_request, ev_first_keycode, ev_count) & io
        pure ()

      (XE.ButtonEvent {..}) → do
        pure ()

      (XE.MotionEvent {..}) → do
        pure ()

      (XE.CrossingEvent {..}) → do
        pure ()

      (XE.ConfigureRequestEvent {..}) → do
        pure ()

      (XE.ConfigureEvent {..}) → do
        pure ()

      (XE.PropertyEvent {..}) → do
        pure ()

      (XE.ClientMessageEvent {..}) → do
        pure ()

      _ → pure ()

main = launch


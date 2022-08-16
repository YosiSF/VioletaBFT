--IPFS
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE RankNTypes #-}
  {-# LANGUAGE TypeFamilies #-}
  {-# LANGUAGE FlexibleInstances #-}
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE TypeFamilies #-}

  import Web.Scotty.Core
  import Web.Scotty.Internal.Types
  import Web.Scotty.Internal.Types (ActionM)
  import Web.Scotty.Internal.Types (ActionError)
  import Web.Scotty.Internal.Types (Options)


  import qualified Data.Text.Lazy as Text.LazyText
  import qualified Data.Text.Lazy.Encoding as Text.LazyText.Encoding
  import qualified Data.ByteString.Lazy as ByteString.Lazy

  import qualified Data.Text as Text
  import qualified Data.ByteString as ByteString
  import qualified Data.ByteString.Char8 as ByteString.Char8

  import qualified Data.Text.Encoding as Text.Encoding
  import qualified Data.ByteString.Base64 as Base64


  import qualified Data.Aeson as Aeson
  import qualified Data.Aeson.Types as Aeson.Types

  import qualified Data.HashMap.Strict as HashMap.Strict
  import qualified Data.Vector as Vector


  import qualified Data.Text.Lazy.IO as Text.LazyText.IO
  import qualified Data.Text.IO as Text.IO




  import qualified Data.Text.Serialize as Text.Serialize
  import qualified Data.Text.Serialize.Text as Text.Serialize.Text
  import qualified Data.Text.Serialize.Text.Lazy as Text.Serialize.Text.Lazy
  import qualified Data.Text.Serialize.Binary as Text.Serialize.Binary
  import qualified Data.Text.Serialize.Binary.Lazy as Text.Serialize.Binary.Lazy







{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import Web.Scotty.Coreturn Configure
import Web.Scotty.Coreturn.Types
import Web.Scotty.Coreturn.Types (ActionM)


import qualified Data.Text.Lazy as Text.LazyText
import qualified Data.Text.Lazy.Encoding as Text.LazyText.Encoding


import qualified Data.Text as Text
import qualified Data.ByteString as ByteString


module Web.Scotty.Coreturn.IPFS.Types where
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text.Encoding as Text.Encoding


import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.HashMap.Strict as HashMap.Strict
import qualified Data.Vector as Vector


import qualified Data.Text.Lazy.IO as Text.LazyText.IO
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Serialize as Text.Serialize





module Dessin.Main where
  import Control.Monad (void)
  import Graphics.UI.Gtk
  import Graphics.UI.Gtk.Gdk.Events
  import Graphics.Rendering.Cairo
  import Dessin.Types
  import Dessin.Drawing

  main :: IO ()

  Event
    :: MonadIO m
    => m Event
  Event = liftIO $ do
    event <- get
    return event

  main = do
    case event of
      MouseButtonDown -> return MouseButtonDown




   <- getState
  case event of
    MouseButtonDown -> return MouseButtonDown
    MouseButtonUp -> return MouseButtonUp
    MouseMotion -> return MouseMotion
    KeyPress -> return KeyPress
    KeyRelease -> return KeyRelease
    Configure -> return Configure
    Expose -> return Expose
     where
      getState = do
        initGUI
        window <- windowNew
        cairo <- newCairoContext window

        on window exposeEvent $ liftIO $ do
          renderWith cairo $ do
            setSourceRGB 0 0 0
            paint

        on window configureEvent $ liftIO $ do
          renderWith cairo $ do
            setSourceRGB 0 0 0
            paint

        on window keyPressEvent $ liftIO $ do
          renderWith cairo $ do
            setSourceRGB 0 0 0
            paint

        canvas <- drawingAreaNew
        on canvas exposeEvent $ tryEvent $ do
          let x1 = fromIntegral (fromIntegral <$> getState)

          Nand x1 x1
          return True
           renderWith cairo $ do
            setSourceRGB 0 0 0
            paint 0 0 0x0100007f
            renderWith cairo $ do
              setSourceRGB 0 0 0
              paint
                renderWith cairo $ do

  on window keyReleaseEvent $ liftIO $ do
    renderWith cairo $ do
      setSourceRGB 0 0 0 0x0100007f
      paint 0 0 0x0100007f
      renderWith cairo $ do
        setSourceRGB 0 0 0 0x0100007f
        paint
        renderWith cairo $ do
          setSourceRGB 0 0 0 0x0100007f
          paint
          renderWith cairo $ do
            setSourceRGB 0 0 0 0x0100007f
            paint
            renderWith cairo $ do
              setSourceRGB 0 0 0 0x0100007f
              paint

  on window mouseButtonPressEvent $ tryEvent $ do
    (x, y) <- eventCoordinates
    liftIO $ do
      renderWith cairo $ do
        setSourceRGB 0 0 0 0x0100007f
        paint
        renderWith cairo $ do
          setSourceRGB 0 0 0 0x0100007f
          paint
          renderWith cairo $ do

  on window mouseButtonReleaseEvent $ tryEvent $ do
    (x, y) <- eventCoordinates
    liftIO $ do
      renderWith cairo $ do
        setSourceRGB 0 0 0 0x0100007f
        paint
        renderWith cairo $ do
          setSourceRGB 0 0 0 0x0100007f
          paint
          renderWith cairo $ do

  on window mouseMotionEvent $ tryEvent $ do
    (x, y) <- eventCoordinates
    module Main where
      import Control.Monad (void)
      import Graphics.UI.Gtk
      import Graphics.UI.Gtk.Gdk.Events


      main :: IO ()
      main = do

        case event of
          MouseButtonDown -> return MouseButtonDown
          MouseButtonUp -> return MouseButtonUp
          MouseMotion -> return MouseMotion
          KeyPress -> return KeyPress
          KeyRelease -> return KeyRelease
          Configure -> return Configure
          CursorMoved -> return CursorMoved

on window mouseButtonMoveEvent $ tryEvent $ do

  (x, y) <- eventCoordinates
  module Main where
    import Control.Monad (void)
    import Graphics.UI.Gtk
    import Graphics.UI.Gtk.Gdk.Events


    main :: IO ()
    main = do

      case event of
        MouseButtonDown -> return MouseButtonDown
        MouseButtonUp -> return MouseButtonUp
        MouseMotion -> return MouseMotion

        KeyPress -> return KeyPress
        KeyRelease -> return KeyRelease


        Configure -> return Configure











           y1 = fromIntegral (fromIntegral <$> getState)
          let x2 = fromIntegral (fromIntegral <$> getState)



          let y2 = fromIntegral (fromIntegral <$> getState)


          liftIO $ do
            renderWith cairo $ do
              setSourceRGB 0 0 0
              rectangle 0 0 500 500
              fill
              setSourceRGB 0 1 0
              rectangle x1 y1 x2 y2
              filling
              stroke
              showPage
              return True
        on canvas buttonPressEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True
        on canvas buttonReleaseEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True
        on canvas motionNotifyEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True
        on canvas keyPressEvent $ tryEvent $ do
          key <- eventKeyName
          liftIO $ do
            putStrLn $ "key: " ++ show key
            return True
        on canvas keyReleaseEvent $ tryEvent $ do
          key <- eventKeyName
          liftIO $ do
            putStrLn $ "key: " ++ show key
            return True
        on canvas scrollEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True
        on canvas buttonPressEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True
        on canvas buttonReleaseEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            putStrLn $ "x: " ++ show x
            putStrLn $ "y: " ++ show y
            return True

          liftIO $ do
            renderWithDrawable (castToDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
        on canvas buttonPressEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            renderWithDrawable (castToDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
              setSourceRGB 0 0 0
              rectangle x y x y
              fill
        on canvas buttonReleaseEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            renderWithDrawable (castToDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
              setSourceRGB 0 0 0
              rectangle x y x y
              fill
        on canvas motionNotifyEvent $ tryEvent $ do
          (x, y) <- eventCoordinates
          liftIO $ do
            renderWithDrawable (castToDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
              setSourceRGB 0 0 0
              rectangle x y x y
              fill
        on window deleteEvent $ liftIO mainQuit >> return False
        on window keyPressEvent $ tryEvent $ do
          key <- eventKeyName
          liftIO $ do
            renderWithDrawable (castToDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
              setSourceRGB 0 0 0
              rectangle x y x y
              fill
          return True
        on window keyReleaseEvent $ tryEvent $ do
          key <- eventKeyName
          liftIO $ do
            renderWithDrawable (cast toDrawable canvas) $ do
              setSourceRGB 0 0 0
              paint
              setSourceRGB 0 0 0
              rectangle 0 0 800 600
              fill
              setSourceRGB 0 0 0
              rectangle x y x y
              fill
          return True
        on window keyReleaseEvent $ tryEvent $ do
          key <- eventKeyName
  liftIO $ do


  widgetShowAll window
  on window keyReleaseEvent $ tryEvent $ do




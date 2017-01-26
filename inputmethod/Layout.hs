{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- Example of using a PangoLayout

import Data.IORef
import Data.Monoid ((<>))
import qualified Data.Text as T

import Graphics.Rendering.Cairo (moveTo, Render)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (DrawingArea, widgetShowAll, onWidgetKeyPressEvent,
        iMContextFilterKeypress, onWidgetKeyReleaseEvent,
        iMContextFocusOut, onWidgetFocusOutEvent, iMContextFocusIn,
        onWidgetFocusInEvent, widgetGetWindow, iMContextSetClientWindow,
        onWidgetRealize, onIMContextDeleteSurrounding,
        iMContextSetSurrounding, onIMContextRetrieveSurrounding,
        onIMContextCommit, iMContextGetPreeditString,
        onIMContextPreeditChanged, onIMContextPreeditEnd,
        onIMContextPreeditStart, iMMulticontextNew, onWidgetDraw,
        onWidgetSizeAllocate, widgetQueueDraw, widgetSetSizeRequest,
        containerAdd, drawingAreaNew, mainQuit, onWidgetDestroy, windowNew)
import GI.Gtk.Enums (WindowType(..))
import GI.Pango
       (contextSetFontMap, contextNew, setAttributeEndIndex,
        setAttributeStartIndex, setAttributeKlass, Attribute(..),
        getAttributeEndIndex, getAttributeStartIndex, getAttributeKlass,
        AttrList(..), attrListInsert, attrListNew, Layout, layoutSetWidth,
        layoutNew, layoutSetAttributes, layoutSetText, layoutSetWrap,
        AttrIterator(..), attrIteratorGetAttrs, attrIteratorDestroy)
import GI.PangoCairo (showLayout, fontMapGetDefault)
import GI.Gdk
       (getEventKeyKeyval, getEventKeyState, getRectangleWidth,
        keyvalToUnicode, keyvalName, EventKey)
import GI.Cairo.Structs.Context (Context(..))
import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (Ptr, castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.GI.Base.Constructible (Constructible(..))
import GI.Pango.Enums (WrapMode(..))
import Data.GI.Base.ManagedPtr (withManagedPtr, wrapPtr)
import Data.GI.Base.BasicTypes (wrappedPtrCopy)

loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit,\
        \ sed do eiusmod tempor incididunt ut labore et dolore magna\
        \ aliqua. Ut enim ad minim veniam, quis nostrud exercitation\
        \ ullamco laboris nisi ut aliquip ex ea commodo consequat.\
        \ Duis aute irure dolor in reprehenderit in voluptate\
        \ velit esse cillum dolore eu fugiat nulla pariatur.\
        \ Excepteur sint occaecat cupidatat non proident, sunt in culpa\
        \ qui officia deserunt mollit anim id est laborum."

data Buffer = Buffer T.Text Int

defaultBuffer = Buffer loremIpsum (T.length loremIpsum)

displayBuffer (Buffer str pos) =
  before <> "<CURSOR>" <> after
  where (before,after) = T.splitAt pos str

displayBufferPreedit (Buffer str pos) preeditStr preeditPos =
  before <> "[" <> prebefore <> "<CURSOR>" <> preafter <> "]" <> after
  where (before,after) = T.splitAt pos str
        (prebefore, preafter) = T.splitAt preeditPos preeditStr

insertStr new (Buffer str pos) = Buffer (before<>new<>after) (pos+T.length new)
  where (before,after) = T.splitAt pos str

deleteChar b@(Buffer str 0) = b
deleteChar (Buffer str pos) = Buffer (T.init before <> after) (pos-1)
  where (before,after) = T.splitAt pos str

moveLeft b@(Buffer str pos) | pos==0 = b
                            | otherwise = Buffer str (pos-1)

moveRight b@(Buffer str pos) | pos==T.length str = b
                             | otherwise = Buffer str (pos+1)

attrListNewFromList :: MonadIO m => [Attribute] -> m AttrList
attrListNewFromList list = do
    al <- attrListNew
    mapM_ (attrListInsert al) list
    return al

foreign import ccall "pango_attr_list_get_iterator" pango_attr_list_get_iterator :: 
    Ptr AttrList -> IO (Ptr AttrIterator)

attrListNewToList :: MonadIO m => AttrList -> m [Attribute]
attrListNewToList list = liftIO $ withManagedPtr list $ \listPtr -> do
  it <- pango_attr_list_get_iterator listPtr >>= wrapPtr AttrIterator
  attrs <- attrIteratorGetAttrs it
  attrIteratorDestroy it
  return attrs

main = do
  Gtk.init Nothing

  -- Create the main window.
  win <- windowNew WindowTypeToplevel
  onWidgetDestroy win mainQuit
  -- Create a drawing area in which we can render text.
  area <- drawingAreaNew
  containerAdd win area
  widgetSetSizeRequest area 100 100

  -- Our widget's data
  buffer <- newIORef defaultBuffer

  preeditRef <- newIORef Nothing

  -- Create a Cairo Context that contains information about the current font,
  -- etc.
  fm <- fontMapGetDefault
  ctxt <- contextNew
  contextSetFontMap ctxt fm
  lay <- layoutNew ctxt
  layoutSetWrap lay WrapModeWord

  let relayout = do
          buffer@(Buffer _ cursor) <- readIORef buffer
          preedit <- readIORef preeditRef
          case preedit of
              Nothing -> do
                  layoutSetText lay (displayBuffer buffer) (-1)
                  layoutSetAttributes lay Nothing
              Just (str,attrs,pos) -> do
                  layoutSetText lay (displayBufferPreedit buffer str (fromIntegral pos)) (-1)
                  mapM (shiftAttribute (cursor + 1)) attrs
                        >>= attrListNewFromList >>= layoutSetAttributes lay . Just
          widgetQueueDraw area

  relayout

  -- Wrap the layout to a different width each time the window is resized.
  onWidgetSizeAllocate area $ \r -> do
    w <- getRectangleWidth r
    layoutSetWidth lay (fromIntegral w)

  -- Setup the handler to draw the layout.
  onWidgetDraw area $ \ctx@(Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
    updateArea area lay ctx
    return True

  -- Set up input method
  im <- iMMulticontextNew

  onIMContextPreeditStart im $ do
      writeIORef preeditRef (Just ("",[],0))
      relayout
  onIMContextPreeditEnd im $ do
      writeIORef preeditRef Nothing
      relayout
  onIMContextPreeditChanged im $ do
      writeIORef preeditRef . Just =<< (\(t, a, n) -> (t, , n) <$> attrListNewToList a) =<< iMContextGetPreeditString im
      relayout
  onIMContextCommit im $ \str -> do
      modifyIORef buffer (insertStr str)
      relayout
  onIMContextRetrieveSurrounding im $ do
      Buffer text pos <- readIORef buffer
      iMContextSetSurrounding im text (-1) (fromIntegral pos)
      return True
  onIMContextDeleteSurrounding im $ \off nchars -> do
      putStrLn $ "delete-surrounding("++show off++","++show nchars++")"
      return False

  onWidgetRealize win $
      iMContextSetClientWindow im =<< widgetGetWindow win
  onWidgetFocusInEvent win $ \e -> liftIO (iMContextFocusIn  im) >> return False
  onWidgetFocusOutEvent win $ \e -> liftIO (iMContextFocusOut im) >> return False
  onWidgetKeyReleaseEvent win $ \e -> iMContextFilterKeypress im e
  onWidgetKeyPressEvent win $ \e ->  do
    imHandled <- iMContextFilterKeypress im e
    if imHandled then return True else do
       mod <- interpretKeyPress e
       case mod of
           Just f -> liftIO $ modifyIORef buffer f >> relayout >> return True
           Nothing -> return False

  widgetShowAll win
  Gtk.main

updateArea :: DrawingArea -> Layout -> Context -> Render ()
updateArea area lay ctx = do
    moveTo 0 0
    showLayout ctx lay

interpretKeyPress :: EventKey -> IO (Maybe (Buffer -> Buffer))
interpretKeyPress e = do
    modifiers <- getEventKeyState e
    if modifiers /= [] then return Nothing else do
        keyVal <- getEventKeyKeyval e
        keyName <- keyvalName keyVal
        keyChar <- toEnum . fromIntegral <$> keyvalToUnicode keyVal
        case keyChar of
            '\0' ->
                case keyName of
                    Just "Left" -> returnJust moveLeft
                    Just "Right" -> returnJust moveRight
                    Just "BackSpace" -> returnJust deleteChar
                    _ -> return Nothing
            ch -> do
                -- This does not appear to get called; the IM handles
                -- unmodified keypresses.
                liftIO $ putStrLn "Literal character not handled by IM"
                returnJust (insertStr $ T.singleton ch)
    where returnJust = return . Just

shiftAttribute :: Int -> Attribute -> IO Attribute
shiftAttribute x attr = do
    start <- getAttributeStartIndex attr
    end <- getAttributeEndIndex attr
    newAttr <- wrappedPtrCopy attr
    setAttributeStartIndex newAttr (start + fromIntegral x)
    setAttributeEndIndex newAttr (end + fromIntegral x)
    return newAttr

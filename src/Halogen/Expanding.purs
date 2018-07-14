module Halogen.Expanding
    ( expandingComponent
    , Query(..)
    , main
    ) where

import Prelude

import Control.Bind (bindFlipped)
import Control.Comonad (extract)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Web.Event.Event (Event)
import Web.HTML (window)
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (toElement) as HTMLElement
import Web.HTML.Window (document)
import Web.DOM.Document (createElement)
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent)
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.Util.TextCursor (Direction(..), TextCursor(..), content, empty)
import Web.Util.TextCursor.Element (TextCursorElement, setTextCursor, textCursor)
import Web.Util.TextCursor.Element.Type (read)
import Data.Bifunctor (lmap)
import Data.Foldable (traverse_)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), stripPrefix, stripSuffix)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Halogen (AttrName(..))
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Set TextCursor a
  | Raising a
  | PreventDefault Event (Query a)
  | NoOp a
  | Update a

foreign import computedStyle :: Element -> Effect String
foreign import fixStyle :: Element -> Effect Unit

-- | Closely based on https://stackoverflow.com/a/7168967
testWidth :: HTMLInputElement -> String -> Effect (Maybe Number)
testWidth input val = do
  tmp <- createElement "div" <<< HTMLDocument.toDocument =<< document =<< window
  let tmpNode = Element.toNode tmp

  setTextContent val tmpNode

  let inputHTMLElement = HInput.toHTMLElement input
  let inputElement = HTMLElement.toElement inputHTMLElement
  let inputNode = Element.toNode inputElement
  styl <- computedStyle inputElement
  Element.setAttribute "style" styl tmp
  fixStyle tmp

  parentNode inputNode >>= traverse \parNode -> do
    _ <- appendChild tmpNode parNode
    width <- Element.clientWidth tmp
    _ <- removeChild tmpNode parNode
    pure width

type Settings =
  { min :: Number
  , max :: Number
  , padding :: Number
  -- , blurred :: Maybe Number
  }


obtainInput :: Maybe Element -> Maybe HTMLInputElement
obtainInput = bindFlipped HInput.fromElement

obtainInputTC :: Maybe Element -> Maybe TextCursorElement
obtainInputTC = bindFlipped read

expandingComponent :: forall m.
  MonadEffect m =>
  Settings ->
  H.Component HH.HTML Query TextCursor TextCursor m
expandingComponent settings =
  H.component
    { initialState: Tuple (ceil settings.min)
    , render
    , eval
    , receiver: HE.input Set
    , initializer: Just (Update unit)
    , finalizer: Nothing
    }
  where
    label = H.RefLabel "textcursor-component" :: H.RefLabel
    render :: Tuple Int TextCursor -> H.ComponentHTML Query () m
    render (Tuple w v) = HH.input
      [ HP.ref label -- give it a label
      , HP.value (content v) -- set the value
      , HP.attr (AttrName "style") ("width: " <> show w <> "px") -- set the width
      , HE.onInput (HE.input_ Raising) -- notify parent on input events
      , HE.onInput (HE.input_ Raising)
      , HE.onClick (HE.input_ Raising)
      , HE.onKeyUp (HE.input_ Raising)
      , HE.onFocus (HE.input_ Raising)
      -- , HE.onKeyPress (HE.input (\e u -> PreventDefault (keyboardEventToEvent e) (NoOp u)))
      ]

    withEl h = H.getRef label >>= obtainInputTC >>> traverse_ h

    eval :: Query ~> H.HalogenM (Tuple Int TextCursor) Query () TextCursor m
    -- When an input event occurs, get the value and notify the parent
    eval (NoOp next) = pure next
    eval (PreventDefault e next) = do
      v <- H.gets extract
      withEl \el ->
        H.liftEffect $ setTextCursor v el
      eval next
    eval (Raising next) = next <$ do
      v <- H.gets extract
      withEl \el -> do
        v' <- H.liftEffect $ textCursor el
        H.liftEffect $ setTextCursor v el
        H.raise v'
    -- Set the input value in state and update the size
    eval (Set v next) = do
      withEl \el -> H.liftEffect $ setTextCursor v el
      H.modify (_ $> v) *> eval (Update next)
    -- Update the size of the input to correspond to the value it will have
    -- on the next render
    eval (Update next) = next <$ do
      H.getRef label >>= obtainInput >>> traverse_ \el -> do
        v <- H.gets extract
        H.liftEffect (testWidth el (content v)) >>= traverse_ \w' -> do
          let
            w = ceil
              $ max settings.min
              $ min settings.max
              $ w' + settings.padding
          H.liftEffect $ HInput.setWidth w el
          H.modify (lmap (const w))
      pure unit

data DemoQuery a
  = Reset TextCursor a
  | Receive TextCursor a

type DemoSlots = ( tc :: H.Slot Query TextCursor Unit )

demo :: forall m.
  MonadEffect m =>
  H.Component HH.HTML DemoQuery Unit Void m
demo =
  H.component
    { initialState: const nov
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    nov = TextCursor
      { before: "before"
      , selected: "(selected)"
      , after: "after"
      , direction: None
      }
    com = expandingComponent { min: 50.0, max: 300.0, padding: 15.0 }

    update = H.put >>> (_ *> inform)
    inform = do
      TextCursor r <- H.get
      H.liftEffect $ log $ unsafeCoerce [r.before, r.selected, r.after]

    render :: TextCursor -> H.ComponentHTML DemoQuery DemoSlots m
    render s = HH.div_ [HH.slot (SProxy :: SProxy "tc") unit com s (HE.input Receive)]

    eval :: DemoQuery ~> H.HalogenM TextCursor DemoQuery DemoSlots Void m
    eval (Reset v a) = a <$ do
      update v
    eval (Receive v a) = a <$ do
      let ignore = isJust <<< stripPrefix (Pattern "ignore: ") <<< content
      prev <- H.get
      when (not (ignore prev && ignore v) || content v == content prev) do
        update v
      when (isJust $ stripSuffix (Pattern "reset") $ content v) do
        eval (Reset empty unit)

main :: Effect Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit

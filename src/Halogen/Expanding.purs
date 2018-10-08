module Halogen.Expanding where

import Prelude

import CSS as CSS
import Control.Bind (bindFlipped)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.State (class MonadState)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.String (Pattern(..), contains)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prim.Row as Row
import Record as Record
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent)
import Web.Event.Event (Event)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument) as HTMLDocument
import Web.HTML.HTMLElement (toElement, blur, focus) as HTMLElement
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HInput
import Web.HTML.Window (document)
import Web.Util.TextCursor (Direction(..), TextCursor(..), _selected, content, empty, single)
import Web.Util.TextCursor.Element (TextCursorElement, setTextCursor, textCursor, toHTMLElement)
import Web.Util.TextCursor.Element.Type (read)

data Query a
  = Input (Tuple Settings Blurry) a
  | Set Blurry a
  | Get (Blurry -> a)
  | Raising (Maybe Boolean) a
  | PreventDefault Event a
  | Resize a

derive instance functorQuery :: Functor Query

set :: Blurry -> Free Query Unit
set b = liftF (Set b unit)

get :: Free Query Blurry
get = liftF (Get identity)

modify :: (Blurry -> Blurry) -> Free Query Blurry
modify f = do
  v <- get
  let v' = f v
  set v'
  pure v'

modify_ :: (Blurry -> Blurry) -> Free Query Unit
modify_ f = void $ modify f

blur :: Free Query Unit
blur = modify_ (toString >>> Blurred)

notify :: Free Query Unit
notify = liftF (Raising Nothing unit)

qRaising :: forall a. Maybe Boolean -> a -> Free Query a
qRaising mb a = liftF (Raising mb a)

foreign import computedStyle :: Element -> Effect String

styleReset :: String
styleReset = """;
max-width: none;
min-width: 0px;
width: auto;
position: static;
white-space: pre;
"""

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

  parentNode inputNode >>= traverse \parNode -> do
    _ <- appendChild tmpNode parNode
    Element.setAttribute "style" (styl <> styleReset) tmp
    width <- Element.clientWidth tmp
    log =<< computedStyle tmp
    _ <- removeChild tmpNode parNode
    pure width

type Settings =
  { classes :: Array H.ClassName
  , style :: CSS.CSS
  , type_ :: HP.InputType
  , padding ::
    { focused :: Exists CSS.Size
    , blurred :: Exists CSS.Size
    }
  }

obtainInput :: Maybe Element -> Maybe HTMLInputElement
obtainInput = bindFlipped HInput.fromElement

obtainInputTC :: Maybe Element -> Maybe TextCursorElement
obtainInputTC = bindFlipped read

data Blurry
  = Blurred String
  | Focused TextCursor

derive instance eqBlurry :: Eq Blurry

instance showBlurry :: Show Blurry where
  show (Blurred s) = "(Blurred " <> show s <> ")"
  show (Focused tc) = "(Focused " <> show tc <> ")"

isBlurred :: Blurry -> Boolean
isBlurred (Blurred _) = true
isBlurred (Focused _) = false

isFocused :: Blurry -> Boolean
isFocused = not isBlurred

toString :: Blurry -> String
toString (Blurred v) = v
toString (Focused tc) = content tc

toTC :: Blurry -> TextCursor
toTC (Blurred v) = single _selected v
toTC (Focused tc) = tc

fromTC :: Boolean -> TextCursor -> Blurry
fromTC true tc = Focused tc
fromTC false tc = Blurred (content tc)

type State =
  { value :: Blurry
  , focus :: Boolean
  , size :: Number
  , settings :: Settings
  }

guardedModify :: forall s t r' r m.
  Eq t =>
  IsSymbol s =>
  Row.Cons s t r' r =>
  Row.Lacks s r' =>
  MonadState (Record r) m =>
  SProxy s -> t -> m Unit
guardedModify s v' = do
  v <- H.gets (Record.get s)
  when (v /= v') (H.modify_ (Record.set s v'))

expandingComponent :: forall m.
  MonadEffect m =>
  H.Component HH.HTML (Free Query) (Tuple Settings Blurry) Blurry m
expandingComponent =
  H.component
    { initialState: \(Tuple settings value) ->
      { settings, value
      , focus: false
      , size: 0.0
      }
    , render
    , eval
    , receiver: HE.input (compose liftF <<< Input)
    , initializer
    , finalizer: Nothing
    }
  where
    initializer :: Maybe (Free Query Unit)
    initializer = Just do
      modify_ identity
      liftF (Resize unit)
    label = H.RefLabel "textcursor-component" :: H.RefLabel
    render :: State -> H.ComponentHTML (Free Query) () m
    render s = HH.input
      [ HP.ref label -- give it a label
      , HP.value (toString s.value) -- set the value
      , HP.type_ s.settings.type_
      , HP.classes s.settings.classes
      , HCSS.style do
          s.settings.style
          let
            padding = if s.focus
              then s.settings.padding.focused
              else s.settings.padding.blurred
          CSS.width $ padding # runExists \(CSS.Size sz) -> CSS.Size $
            CSS.fromString ("calc(" <> show s.size <> "px + ") <> sz <> CSS.fromString ")"
      , HE.onInput (HE.input_ (qRaising Nothing)) -- notify parent on input events
      , HE.onInput (HE.input_ (qRaising Nothing))
      , HE.onClick (HE.input_ (qRaising Nothing))
      , HE.onKeyUp (HE.input_ (qRaising Nothing))
      , HE.onFocus (HE.input_ (qRaising (Just true)))
      , HE.onBlur (HE.input_ (qRaising (Just false)))
      -- , HE.onKeyPress (HE.input (\e u -> PreventDefault (keyboardEventToEvent e) (NoOp u)))
      ]

    withEl :: forall a.
      (TextCursorElement -> H.HalogenM State (Free Query) () Blurry m a) ->
      H.HalogenM State (Free Query) () Blurry m (Maybe a)
    withEl h = H.getRef label >>= obtainInputTC >>> traverse h

    check = withEl \el -> do
      focus <- H.gets _.focus
      fromTC focus <$> H.liftEffect (textCursor el)
    resetTo v = void $ withEl \el -> H.liftEffect $ do
      setTextCursor (toTC v) el
      case v of
        Blurred _ -> HTMLElement.blur (toHTMLElement el)
        Focused _ -> HTMLElement.focus (toHTMLElement el)
    reset = resetTo =<< H.gets _.value

    eval :: Free Query ~> H.HalogenM State (Free Query) () Blurry m
    eval a = foldFree eval1 a

    eval1 :: Query ~> H.HalogenM State (Free Query) () Blurry m
    -- When an input event occurs, get the value and notify the parent
    eval1 (PreventDefault e next) = next <$ reset
    eval1 (Get next) = H.gets _.value <#> next
    eval1 (Raising focus next) = next <$ do
      focus # traverse_ (guardedModify (SProxy :: SProxy "focus"))
      mv' <- check
      v <- H.gets _.value
      resetTo v
      when (mv' /= Just v) (mv' # traverse_ H.raise)
    eval1 (Input (Tuple settings v) next) = do
      --guardedModify (SProxy :: SProxy "settings") settings
      H.modify_ _ { settings = settings }
      eval1 (Set v next)
    -- Set the input value in state and update the size
    eval1 (Set v next) = do
      resetTo v
      guardedModify (SProxy :: SProxy "value") v
      eval1 (Resize next)
    -- Update the size of the input to correspond to the value it will have
    -- on the next render
    eval1 (Resize next) = next <$ do
      H.getRef label >>= obtainInput >>> traverse_ \el -> do
        v <- H.gets _.value
        H.liftEffect (testWidth el (toString v)) >>= traverse_ \w -> do
          guardedModify (SProxy :: SProxy "size") w

data DemoQuery a
  = Reset Blurry a
  | Receive Blurry a
  | Focus a
  | FontSize Number a

type DemoSlots = ( tc :: H.Slot (Free Query) Blurry Unit )

demo :: forall m.
  MonadEffect m =>
  H.Component HH.HTML DemoQuery Unit Void m
demo =
  H.component
    { initialState: const
      { value: nov
      , fontSize: 15.0
      }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    nov = Focused $ TextCursor
      { before: "before"
      , selected: "(selected)"
      , after: "after"
      , direction: None
      }
    settings s =
      { classes: []
      , type_: HP.InputText
      , padding:
        { focused: mkExists $ 2.0 # CSS.em
        , blurred: mkExists $ 1.0 # CSS.em
        }
      , style: do
          let focus = isFocused s.value
          CSS.minWidth $ if focus then 7.0 # CSS.em else 3.0 # CSS.em
          CSS.maxWidth $ if focus then 24.0 # CSS.em else 20.0 # CSS.em
          CSS.fontSize $ s.fontSize # CSS.px
          CSS.key (CSS.fromString "text-overflow") "ellipsis"
      }

    update v = H.modify_ _ { value = v } *> inform
    inform =
      H.gets _.value >>= log <<< case _ of
        Blurred s -> s
        Focused (TextCursor r) ->
          unsafeCoerce [r.before, r.selected, r.after]

    render :: { value :: Blurry, fontSize :: Number } -> H.ComponentHTML DemoQuery DemoSlots m
    render s = HH.div_
      [ HH.input
        [ HP.type_ InputRange, HP.value (show s.fontSize)
        , HP.min 5.0, HP.max 50.0
        , HE.onValueInput \v -> FontSize <$> (fromString v :: Maybe Number) <@> unit
        ]
      , HH.br_
      , HH.slot (SProxy :: SProxy "tc") unit expandingComponent
          ((Tuple <$> settings <*> _.value) s)
          (HE.input Receive)
      , HH.button [ HE.onClick (HE.input_ Focus) ] [ HH.text "Focus" ]
      ]

    eval :: DemoQuery ~> H.HalogenM { value :: Blurry, fontSize :: Number } DemoQuery DemoSlots Void m
    eval (Focus a) = a <$ do
      H.gets _.value >>= toString >>> single _selected >>> Focused >>> update
    eval (Reset v a) = a <$ do
      update v
    eval (FontSize s a) = a <$ H.modify_ _ { fontSize = s }
    eval (Receive v a) = a <$ do
      prev <- H.gets _.value
      let
        mentions p = contains p <<< toString
        ignore = mentions (Pattern "stop")
        stayhere =
          case v of
            Blurred s
              | contains (Pattern "stay here") s
              || contains (Pattern "stay here") (toString prev) -> true
            _ -> false
        samesies = toString v == toString prev
        updating =
          if stayhere then false else
          -- Always allow just the cursor to move
          if samesies then true else
          -- But do not update if the new and old are both ignored strings ...
          if ignore prev && ignore v then false else
          true
      when updating do
        update v
        when (mentions (Pattern "reset") v) do
          update $ Focused empty
        when (mentions (Pattern "blur") v && isFocused prev && not samesies) do
          update $ Blurred $ toString v
        when (mentions (Pattern "select all") v) do
          update $ Focused $ single _selected $ toString v

main :: Effect Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit

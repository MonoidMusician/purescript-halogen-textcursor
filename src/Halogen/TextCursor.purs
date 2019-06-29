module Halogen.TextCursor
    ( textCursorComponent
    , Query(..)
    , TCInputType(..)
    , main
    ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.UIEvent.FocusEvent (toEvent) as FocusEvent
import Web.UIEvent.KeyboardEvent (toEvent) as KeyboardEvent
import Web.UIEvent.MouseEvent (toEvent) as MouseEvent
import Web.Util.TextCursor (Direction(None), TextCursor(TextCursor), content)
import Web.Util.TextCursor.Element (setTextCursor, textCursor, validate')
import Web.Util.TextCursor.Element.Type (read, readEventTarget)

data Query a
  = Init a
  | FromEvent TextCursor a
  | Update Event a
  | FromOutside TextCursor a

data TCInputType
  = TCTextArea
  | TCInput
  | TCEmail
  | TCSearch
  | TCUrl

toInputType :: TCInputType -> Maybe InputType
toInputType = case _ of
  TCTextArea -> Nothing
  TCInput -> Just InputText
  TCEmail -> Just InputEmail
  TCSearch -> Just InputSearch
  TCUrl -> Just InputUrl

textCursorComponent :: forall m.
  MonadEffect m =>
  TCInputType ->
  H.Component HH.HTML Query TextCursor TextCursor m
textCursorComponent typ =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = eval
      , handleQuery = map pure <<< eval
      , receive = pure <<< (#) unit <<< FromOutside
      , initialize = Just (Init unit)
      }
    }
  where
    label = H.RefLabel "textcursor-component" :: H.RefLabel
    render :: TextCursor -> H.ComponentHTML (Query Unit) () m
    render = case toInputType typ of
      Nothing -> \tc ->
        HH.textarea
          [ HP.ref label
          , HP.value (content tc)
          ]
      Just ty -> \tc ->
        HH.input
          [ HP.ref label
          , HP.type_ ty
          , HP.value (content tc)
          , HE.onInput (pure <<< (#) unit <<< (Update <<< identity))
          , HE.onClick (pure <<< (#) unit <<< (Update <<< MouseEvent.toEvent))
          , HE.onKeyUp (pure <<< (#) unit <<< (Update <<< KeyboardEvent.toEvent))
          , HE.onFocus (pure <<< (#) unit <<< (Update <<< FocusEvent.toEvent))
          ]

    eval :: Query ~> H.HalogenM TextCursor (Query Unit) () TextCursor m
    eval (Init next) = do
      tc <- H.get
      eval (FromOutside tc next)
    eval (FromEvent tc next) = next <$ do
      cur <- H.get
      when (cur /= tc) do
        H.put tc <* H.raise tc
    eval (Update e next) = next <$ runMaybeT do
      elem <- MaybeT $ H.liftEffect $ validate' (readEventTarget e)
      tc <- H.liftEffect $ textCursor elem
      lift $ eval (FromEvent tc unit)
    eval (FromOutside tc next) = next <$ runMaybeT do
      e <- MaybeT $ H.getRef label
      elem <- MaybeT $ H.liftEffect $ validate' (read e)
      H.liftEffect $ setTextCursor tc elem
      H.put tc

data DemoQuery a
  = Set TextCursor a
  | Receive TextCursor a

type DemoSlots = ( tc :: H.Slot Query TextCursor Unit )

demo :: forall m.
  MonadEffect m =>
  H.Component HH.HTML DemoQuery Unit Void m
demo =
  H.mkComponent
    { initialState: const nov
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = eval, handleQuery = map pure <<< eval }
    }
  where
    nov = TextCursor
      { before: "before"
      , selected: "(selected)"
      , after: "after"
      , direction: None
      }
    com = textCursorComponent TCInput

    update = H.put >>> (_ *> inform)
    inform = do
      TextCursor r <- H.get
      H.liftEffect $ log $ unsafeCoerce [r.before, r.selected, r.after]

    render :: TextCursor -> H.ComponentHTML (DemoQuery Unit) DemoSlots m
    render tc = HH.slot (SProxy :: SProxy "tc") unit com tc (pure <<< (#) unit <<< Receive)

    eval :: DemoQuery ~> H.HalogenM TextCursor (DemoQuery Unit) DemoSlots Void m
    eval (Set tc a) = a <$ do
      update tc
    eval (Receive tc a) = a <$ do
      update tc
      when (content tc == "reset") do
        eval (Set nov unit)

main :: Effect Unit
main = runHalogenAff $ awaitBody >>= runUI demo unit

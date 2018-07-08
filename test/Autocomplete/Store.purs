module Test.Autocomplete.Store where

import Prelude

import Autocomplete.Store (SuggesterState(SuggesterState), SuggesterAction(AddResults, SetTerms), updateSuggestions)
import Autocomplete.Types (Terms, Suggestions(Ready, Loading))
import Data.Foldable (foldl)
import Data.Map (Map, singleton)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Unit (suite, test, TestSuite)
import Test.Unit.Assert (equal)

runTests :: TestSuite
runTests =
  suite "Autocomplete.Store" do

    test "updateSuggestions returns empty Loading results for an empty store" do
      equal "" $ currentTerms emptyState
      equal (Loading []) $ currentResults emptyState

    test "updateSuggestions returns empty Loading results for an empty store after terms are set" do
      let
        state = updateSuggestions (SetTerms "foo") emptyState
      equal "foo" $ currentTerms state
      equal (Loading []) $ currentResults state

    test "updateSuggestions returns empty Ready results for a store after terms are set and results loaded" do
      let
        fooResults = Ready ["a", "b", "c"]
        state1 = updateSuggestions (SetTerms "foo") emptyState
        state2 = updateSuggestions (AddResults (Tuple "foo" fooResults)) state1
      equal "foo" $ currentTerms state2
      equal fooResults $ currentResults state2

    test "updateSuggestions returns cached results in any order" do
      let
        ops =
          [ Tuple "" (Ready [])
          , Tuple "f" (Ready ["a"])
          , Tuple "b" (Ready ["b"])
          , Tuple "fo" (Ready ["aa"])
          , Tuple "ba" (Ready ["bb"])
          , Tuple "foo" (Ready ["aaa"])
          , Tuple "bar" (Ready ["bbb"])
          ]
        initialState = foldl (flip $ AddResults >>> updateSuggestions) emptyState ops

      state <- liftEffect $ Ref.new initialState

      setTerms state "foo"
      assertResults state $ Ready ["aaa"]

      setTerms state "ba"
      assertResults state $ Ready ["bb"]

      setTerms state "b"
      assertResults state $ Ready ["b"]

      setTerms state ""
      assertResults state $ Ready []

      setTerms state "f"
      assertResults state $ Ready ["a"]

      setTerms state "fo"
      assertResults state $ Ready ["aa"]

      setTerms state "foo"
      assertResults state $ Ready ["aaa"]

      setTerms state ""
      assertResults state $ Ready []

      setTerms state "b"
      assertResults state $ Ready ["b"]

      setTerms state "ba"
      assertResults state $ Ready ["bb"]

      setTerms state "bar"
      assertResults state $ Ready ["bbb"]

setTerms
  :: forall a
   . Ref.Ref (SuggesterState a)
  -> Terms
  -> Aff Unit
setTerms ref terms = liftEffect do
  Ref.modify_ (updateSuggestions (SetTerms terms)) ref

assertResults
  :: forall a
   . Eq a
  => Show a
  => Ref.Ref (SuggesterState a)
  -> Suggestions a
  -> Aff Unit
assertResults ref expected = do
  state <- liftEffect $ Ref.read ref
  equal expected $ currentResults state

currentTerms :: forall a. SuggesterState a -> Terms
currentTerms (SuggesterState s) = s.currentTerms

currentResults :: forall a. SuggesterState a -> Suggestions a
currentResults (SuggesterState s) = s.currentResults

currentStore :: forall a. SuggesterState a -> Map Terms (Suggestions a)
currentStore (SuggesterState s) = s.store

emptyState :: SuggesterState String
emptyState =
  SuggesterState
    { currentTerms: mempty
    , currentResults: mempty
    , termsHistory: mempty
    , store: singleton mempty mempty
    }

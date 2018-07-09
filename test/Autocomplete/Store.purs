module Test.Autocomplete.Store where

import Prelude

import Autocomplete.Store (SuggesterAction(AddResults), SuggesterState(SuggesterState), getSuggestionResults, updateSuggestions)
import Autocomplete.Types (Terms, Suggestions(Ready, Loading))
import Data.Foldable (foldl)
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
      equal (Loading []) $ getSuggestionResults (pure "") emptyState

    test "updateSuggestions returns empty Loading results for an empty store after terms are set" do
      equal (Loading []) $ getSuggestionResults (pure "foo") emptyState

    test "updateSuggestions returns empty Ready results for a store after terms are set and results loaded" do
      let
        fooResults = Ready ["a", "b", "c"]
        state = updateSuggestions (AddResults (Tuple "foo" fooResults)) emptyState
      equal fooResults $ getSuggestionResults (pure "foo") state

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

      assertResults "foo" state $ Ready ["aaa"]
      assertResults "ba" state $ Ready ["bb"]
      assertResults "b" state $ Ready ["b"]
      assertResults "" state $ Ready []
      assertResults "f" state $ Ready ["a"]
      assertResults "fo" state $ Ready ["aa"]
      assertResults "foo" state $ Ready ["aaa"]
      assertResults "" state $ Ready []
      assertResults "b" state $ Ready ["b"]
      assertResults "ba" state $ Ready ["bb"]
      assertResults "bar" state $ Ready ["bbb"]

assertResults
  :: forall a
   . Eq a
  => Show a
  => Terms
  -> Ref.Ref (SuggesterState a)
  -> Suggestions a
  -> Aff Unit
assertResults terms ref expected = do
  state <- liftEffect $ Ref.read ref
  equal expected $ getSuggestionResults (pure terms) state

emptyState :: SuggesterState String
emptyState = SuggesterState mempty

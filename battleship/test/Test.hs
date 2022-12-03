import Game 
import Test.Tasty
import Test.Tasty.QuickCheck 
import Transition

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [game, state]

game :: TestTree
game = testGroup "Game Logic Tests" 
  [
    testProperty  "Init" prop_init_empty,
    testProperty  "Attack - Valid" prop_attack_valid,
    testProperty  "Attack - Plus 1" prop_attack_plus_1,
    testProperty  "Attack - Subset" prop_attack_subset,
    testProperty  "Attack - Previous" prop_attack_prev,
    testProperty  "Attack - OOB" prop_attack_oob,
    testProperty  "Terminate" prop_term,
    testProperty  "IsHit - Hit" prop_ishit_hit,
    testProperty  "IsHit - Miss" prop_ishit_miss,
    testProperty  "AddShip - Valid " prop_add_valid,
    testProperty  "AddShip - Plus 1" prop_add_p1,
    testProperty  "AddShip - Replace" prop_add_replace,
    testProperty  "AddShip - Replace Size" prop_add_same,
    testProperty  "AddShip - OOB" prop_add_oob,
    testProperty  "AddShip - Overlap" prop_add_overlap
  ]

state :: TestTree
state = testGroup "State Tests"
  [
    testProperty  "HandleCharacter - Input" prop_ch_valid,
    testProperty  "HandleCharacter - Input Append" prop_ch_append,
    testProperty  "HandleCharacter - Waiting" prop_ch_nothing,
    testProperty  "HandleBackspace - Input" prop_bkspc_valid,
    testProperty  "HandleBackspace - Trunc" prop_bkspc_trunc,
    testProperty  "HandleBackspace - Empty" prop_bkspc_empty,
    testProperty  "HandleBackspace - Waiting" prop_bkspc_nothing,
    testProperty  "Waiting - Attack Hit" prop_wait_attack_hit,
    testProperty  "Waiting - Attack Miss" prop_wait_attack_miss,
    testProperty  "Waiting - Lose" prop_wait_lose,
    testProperty  "AttackWait - Wait Hit" prop_attackwait_wait_hit,
    testProperty  "AttackWait - Wait Miss" prop_attackwait_wait_miss,
    testProperty  "AttackWait - Win" prop_attackwait_win,
    testProperty  "Attack - AttackWait" prop_attacking_valid,
    testProperty  "Attack - Prev" prop_attacking_prev,
    testProperty  "Attack - Invalid" prop_attacking_invalid
  ]
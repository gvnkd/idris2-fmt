||| Test runner for idris2-fmt using Test.Golden.
module Main

import Test.Golden

||| Main entry point for the test runner.
main : IO ()
main = runner
  [ !((testsInDir "." "idris2-fmt") )
  ]

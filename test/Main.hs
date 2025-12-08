{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import MCP.Server.Auth (HashedPassword (..), defaultDemoCredentialStore, mkHashedPassword, validateCredential)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "Running tests..."
    results <-
        sequence
            [ testValidateCredentialValid
            , testValidateCredentialInvalidPassword
            , testValidateCredentialInvalidUsername
            , testMkHashedPasswordConsistency
            ]
    if and results
        then do
            putStrLn "All tests passed!"
            exitSuccess
        else do
            putStrLn "Some tests failed!"
            exitFailure

-- Test helper
runTest :: String -> Bool -> IO Bool
runTest name result = do
    putStrLn $ "  " ++ name ++ ": " ++ if result then "PASS" else "FAIL"
    return result

-- T016: Test validateCredential with valid credentials
testValidateCredentialValid :: IO Bool
testValidateCredentialValid =
    runTest "validateCredential with valid credentials" $
        validateCredential defaultDemoCredentialStore "demo" "demo123"
            && validateCredential defaultDemoCredentialStore "admin" "admin456"

-- T017: Test validateCredential with invalid credentials
testValidateCredentialInvalidPassword :: IO Bool
testValidateCredentialInvalidPassword =
    runTest "validateCredential with invalid password" $
        not (validateCredential defaultDemoCredentialStore "demo" "wrongpassword")
            && not (validateCredential defaultDemoCredentialStore "admin" "wrongpass")

testValidateCredentialInvalidUsername :: IO Bool
testValidateCredentialInvalidUsername =
    runTest "validateCredential with invalid username" $
        not (validateCredential defaultDemoCredentialStore "nonexistent" "demo123")

-- T018: Test mkHashedPassword consistency
testMkHashedPasswordConsistency :: IO Bool
testMkHashedPasswordConsistency =
    let salt = "test-salt" :: Text
        password = "test-password" :: Text
        hash1 = mkHashedPassword salt password
        hash2 = mkHashedPassword salt password
        hash3 = mkHashedPassword salt "different-password"
        hash4 = mkHashedPassword "different-salt" password
     in runTest "mkHashedPassword consistency" $
            unHashedPassword hash1 == unHashedPassword hash2 -- Same inputs produce same hash
                && unHashedPassword hash1 /= unHashedPassword hash3 -- Different password produces different hash
                && unHashedPassword hash1 /= unHashedPassword hash4 -- Different salt produces different hash

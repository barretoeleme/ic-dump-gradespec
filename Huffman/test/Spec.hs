module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Huffman

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Huffman"
    [ testFrequency
    , testEncode
    , testDecode
    , testRoundTrip
    ]

------------------------------------------------

testFrequency :: TestTree
testFrequency =
    testCase "frequency" $
        frequency "banana"
            @?=
            [('a',3),('b',1),('n',2)]

------------------------------------------------

testEncode :: TestTree
testEncode =
    testCase "encode" $ do

        let texto = "banana"

        let arvore = buildTree (makeLeaves (frequency texto))

        let tabela = codes arvore

        let resultado = encode tabela texto

        assertBool "Não pode ser vazio" (not (null resultado))

------------------------------------------------

testDecode :: TestTree
testDecode =
    testCase "decode" $ do

        let texto = "banana"

        let arvore = buildTree (makeLeaves (frequency texto))

        let tabela = codes arvore

        decode arvore (encode tabela texto)
            @?=
            texto

------------------------------------------------

testRoundTrip :: TestTree
testRoundTrip =
    testCase "round trip" $ do

        let texto = "Algoritmo de Huffman"

        let arvore = buildTree (makeLeaves (frequency texto))

        let tabela = codes arvore

        let comprimido = encode tabela texto

        let resultado = decode arvore comprimido

        resultado @?= texto
module Test where

data Unit = Unit

foo :: Int
foo = 1

bar :: String
bar = "Hello"

baz :: Int -> String
baz i = "hello"

boz :: Int -> Int -> String
boz a b = "hi"


bozo :: String
bozo = boz 1 3

blooz :: Int -> String -> Int -> String
blooz _ str _ = str

bloze :: Int -> String
bloze = blooz 123 "hello"

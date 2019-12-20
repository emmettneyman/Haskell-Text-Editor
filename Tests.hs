module Tests where

    import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
    import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements,
        oneof, frequency, sized, quickCheckWith, stdArgs, maxSize,
        classify,  maxSuccess, listOf, resize, scale, (==>), Property)
    import PieceTable (addChar, delChar, moveTo, empty, PieceTable, flush)
    
    quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
    quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n , maxSize = 100 }
    
    prop_checkInsLen :: Char -> String -> Bool
    prop_checkInsLen ch str = length (flush (addChar ch empty) str) 
                                == length (flush empty str) + 1
    
    prop_checkInsStr :: Char -> String -> Bool
    prop_checkInsStr ch str = 
        flush (addChar ch empty) str == ch : flush empty str
    
    prop_checkAddThenDel :: Char -> String -> Bool
    prop_checkAddThenDel ch str = 
        flush (delChar (addChar ch empty)) str == flush empty str
    
    prop_checkDel :: String -> Property
    prop_checkDel str = not (null str) ==> 
        length (flush (delChar (moveTo 1 empty)) str) == length str - 1
    
    prop_checkDelRandomPos :: Int -> String -> Property
    prop_checkDelRandomPos idx str = length str > idx && idx > 0 ==> 
        length (flush (delChar (moveTo idx empty)) str) == length str - 1
    
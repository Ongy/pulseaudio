
import Data.Char (isSpace, toUpper, toLower)
import Data.List (isPrefixOf, dropWhile, notElem)

hEnums :: [String]
hEnums =
    [ "ContextState"
    , "StreamState"
    , "OperationState"
    , "Direction"
    , "ErrorCode"
    , "SeekMode"
    , "SinkState"
    , "SourceState"
    , "PortAvailable"
    ]

hFlags :: [String]
hFlags =
    [ "ContextFlags"
    , "SubscriptionMask"
    , "SinkFlags"
    , "SourceFlags"
    ]

data CEnum = CEnum String [String] deriving (Eq, Show)

findEnum :: [String] -> ([String], [String])
findEnum xs =
    let ys = dropWhile (not . isPrefixOf "typedef enum") xs
        (zs, ret) = break (\y -> not (null y) && head y == '}' && last y == ';') ys
        filterF zs =
            let as = dropWhile isSpace zs in
                not (null zs) && not ("*" `isPrefixOf` as || "/*" `isPrefixOf` as)
    in (filter filterF zs, ret)

findEnums :: [String] -> [[String]]
findEnums arg =
    let (cur, nxt) = findEnum arg in case nxt of
        [] -> if null cur then [] else [cur]
        _  -> cur : findEnums nxt

cleanIfDefs :: [String] -> [String]
cleanIfDefs xs =
    let (ys, zs) = break (=="#ifdef") xs in
        ys ++ if null zs
                 then []
                 else cleanIfDefs (tail $ dropWhile (/= "#endif") zs)

toCEnum :: [String] -> CEnum
toCEnum (x:xs) =
    let name = words x !! 2
        args = map (\ys -> filter (/= ',') $ words ys !! 0) $ xs
     in CEnum name (cleanIfDefs args)

cNameToConstructor :: String -> String
cNameToConstructor ('P':'A':'_':x:xs) =
    toUpper x : (transform $ map toLower xs)
    where transform [] = []
          transform ('_':y:ys) = toUpper y : transform ys
          transform (y:ys) = y : transform ys

cNameToType :: CEnum -> CEnum
cNameToType (CEnum name args) =
    CEnum (transform name) args
    where transform ('p':'a':'_':x:xs) = toUpper x : transformI xs
          transformI [] = []
          transformI ('_':x:xs) = toUpper x : transformI xs
          transformI (x:xs) = x : transformI xs

printEnum :: CEnum -> IO ()
printEnum (CEnum name args) = do
    putStrLn name
    mapM_ (\arg -> do
            putStr "  "
            putStr arg
            putStr " = "
            putStrLn (cNameToConstructor arg)
          ) args

printDataCons :: [String] -> IO ()
printDataCons (x:xs) = do
    putStr "    = "
    putStrLn $ cNameToConstructor x
    mapM_ (\y -> do
            putStr "    | "
            putStrLn $ cNameToConstructor y
          ) xs

printDataDef :: CEnum -> IO ()
printDataDef (CEnum name args) = do
    putStr "data "
    putStrLn name
    printDataCons args
    putStrLn "    deriving (Eq, Show)\n"

printToInt :: CEnum -> IO ()
printToInt (CEnum name@(x:xs) args) = do
    putType
    mapM_ (\arg -> do
            putName
            putChar ' '
            putStr (cNameToConstructor arg)
            putStr " = #{const "
            putStr arg
            putStrLn "}"
          ) args
    putStr "\n"
    where putType = do
            putName
            putStr " :: "
            putStr name
            putStrLn " -> CInt"
          putName = do
            putChar (toLower x)
            putStr xs
            putStr "ToInt"

printToIntF :: CEnum -> IO ()
printToIntF en@(CEnum name@(x:xs) _) = do
    printToInt en
    putType
    putName
    putStr " = foldFlag "
    putChar (toLower x)
    putStr xs
    putStrLn "ToInt\n"
    where putType = do
            putName
            putStr " :: ["
            putStr name
            putStrLn "] -> CInt"
          putName = do
            putChar (toLower x)
            putStr xs
            putStr "sToInt"

printFromIntE :: CEnum -> IO ()
printFromIntE (CEnum name@(x:xs) args) = do
    putType
    putName
    putStrLn " i"
    mapM_ (\arg -> do
            putStr "    | i == #{const "
            putStr arg
            putStr "} = "
            putStrLn (cNameToConstructor arg)
          ) args
    putStr "    | otherwise = error (\"PA: Unexpeced value @"
    putName
    putStrLn "\" ++ show i)\n"
    where putType = do
            putName
            putStr " :: CInt -> "
            putStrLn name
          putName = do
              putChar (toLower x)
              putStr xs
              putStr "FromInt"

printFromIntF :: CEnum -> IO ()
printFromIntF (CEnum name@(x:xs) args) = do
    putType
    putName
    putStrLn " i ="
    putStrLn "    let"
    let indexed = zip [0..] args
    mapM_ (\(i, arg) -> do
            putStr $ replicate 8 ' '
            putChar 't'
            putStr (show i)
            putStr " = if (i .&. #{const "
            putStr arg
            putStr "} /= 0) then ("
            putStr (cNameToConstructor arg)
            putStrLn ":) else id"
         ) indexed
    putStr "    in "
    mapM_ (\(i, _) -> do
            putChar 't'
            putStr (show i)
            putStr " . "
          ) indexed
    putStrLn "id $ []"
    where putType = do
            putName
            putStr " :: CInt -> ["
            putStr name
            putStr "]\n"
          putName = do
              putChar (toLower x)
              putStr xs
              putStr "sFromInt"

putFoldFun :: IO ()
putFoldFun = do
    putStrLn "foldFlag :: (Foldable t, Num b, Bits b) => (a -> b) -> t a -> b"
    putStrLn "foldFlag fun = foldr ((.|.) . fun) 0"
    putStr "\n"

putHead :: IO ()
putHead = do
    putStrLn "module Sound.Pulse.Def"
    putStrLn "where\n"
    putStrLn "#include <pulse/def.h>\n"
    putStrLn "import Data.Bits (Bits(..))\n"
    putStrLn "import Foreign.C.Types (CInt)\n"
    putFoldFun

printEnumE :: CEnum -> IO ()
printEnumE enum = do
    printDataDef enum
    printToInt enum
    printFromIntE enum

printEnumF :: CEnum -> IO ()
printEnumF enum = do
    printDataDef enum
    printToIntF enum
    printFromIntF enum

main :: IO ()
main = do
    content <- lines <$> readFile "/usr/include/pulse/def.h"
    let allEnums = map (cNameToType . toCEnum) $ findEnums content

    let flags = filter (\(CEnum name _) -> name `elem` hFlags) allEnums
    let enums = filter (\(CEnum name _) -> name `elem` hEnums) allEnums

    putHead
    mapM_ printEnumE enums
    mapM_ printEnumF flags

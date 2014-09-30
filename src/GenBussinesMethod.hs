module GenBussinesMethod where


import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty

import JavaUtils
import Data.Char
import Data.List.Split

data OptionsGenBussinesMethod = OptionsGenBussinesMethod { quickData :: String }


data SimpleFunction = SimpleFunction { simpleName :: String
                                     , simpleReturn :: String
                                     , simpleArguments :: [(String, String)]
                                     }


genBussinesMethod :: OptionsGenBussinesMethod -> IO ()
genBussinesMethod opts = do
    let quick = quickData opts

    let m = genBM . parseQuick $ quick
    putStrLn . prettyPrint $ m


parseQuick :: String -> SimpleFunction
parseQuick s = SimpleFunction parseName parseTypeReturn parseArgs 
    where
        parseName = head . splitWhen (==';') $ s

        parseTypeReturn = head . splitWhen (=='>') . last . splitWhen (==';') $ s

        parseArgs = map parse . splitWhen (==',') . last . splitWhen (=='>') . last . splitWhen (==';') $ s

        parse t = (last . splitWhen (==':') $ t, head . splitWhen (==':') $ t)


genBM :: SimpleFunction -> MemberDecl
genBM sm = if simpleReturn sm == [] then
    MethodDecl [Public] [] Nothing (Ident (simpleName sm)) generateArguments []
            (MethodBody 
                (Just (
                    Block 
                        [])))
    else MethodDecl [Public] [] (Just (genTypeOrRef (simpleReturn sm))) (Ident (simpleName sm)) generateArguments []
            (MethodBody 
                (Just (
                    Block 
                        [])))
    where
        genType t n = if ref t then (genTypeOrRef t, (VarId (Ident n))) else (genTypeOrRef t, (VarId (Ident n)))

        
        genTypeOrRef t = if ref t then RefType (ClassRefType (ClassType [(Ident t, [])])) else PrimType (conv t)

        generateArguments = [FormalParam [] name False typ | (name, typ) <- map (\(x, y) -> genType x y) (simpleArguments sm)]

        ref (t:_) = isUpper t

        conv t = case t of
                    "int"       -> IntT
                    "boolean"   -> BooleanT
                    "long"      -> LongT
                    "short"     -> ShortT
                    "float"     -> FloatT
                    "char"      -> CharT
                    _           -> error $ "Type not found: " ++ t

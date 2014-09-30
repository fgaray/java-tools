module GenGetSet where


import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty

import Data.Maybe
import Data.List (find)
import JavaUtils
import Control.Monad (mapM_)

import Utils


data OptionsGetSet = OptionsGetSet { fileName :: String }

genGetSet :: OptionsGetSet -> IO ()
genGetSet opts = do
    let file = fileName opts
    (Right ast) <- parser compilationUnit `fmap` readFile file
    let body = catMaybes . map getClassBody . getClasses . getBodyClass $ ast
        fieldDecl = concat . map getFieldDecl . map getDecl $ body
        fixed = concat . map fixDecL $ fieldDecl
        sets = filter (noRepeatedMethods methodNames) . map generateSet $ fixed
        gets = filter (noRepeatedMethods methodNames) . map generateGet $ fixed
        methodNames = getMethodNames . concat . map getDecl $ body


    mapM_ putStrLn . (map show) $ sets ++ gets
    mapM_ putStrLn . (map prettyPrint) $ sets ++ gets

    where
        noRepeatedMethods :: [String] -> MemberDecl  -> Bool
        noRepeatedMethods l m = case getMethodName m of
                                    Nothing -> True
                                    Just name -> case find (==name) l of
                                                    Nothing -> True
                                                    Just _  -> False

        


generateGet :: (Type, VarDecl) -> MemberDecl
generateGet (typ, vdc) = 
    (MethodDecl [Public] [] (Just typ) (Ident name) [] [] 
        (MethodBody 
            (Just (
                Block 
                    [BlockStmt 
                        (Return (Just (FieldAccess (PrimaryFieldAccess This (Ident varName)))))]))))
    where
        name = "get" ++ (capitalized $ getNameVar vdc)
        varName = getNameVar vdc


generateSet :: (Type, VarDecl) -> MemberDecl
generateSet (typ, vdc) =
    (MethodDecl [Public] [] Nothing (Ident name) [FormalParam [] typ False (VarId (Ident varName))] [] 
        (MethodBody 
            (Just (
                Block 
                    [BlockStmt 
                        (ExpStmt (Assign (FieldLhs (PrimaryFieldAccess This (Ident varName))) EqualA (ExpName (Name [Ident varName]))))]))))
    where
        name = "set" ++ (capitalized $ getNameVar vdc)
        varName = getNameVar vdc

module JavaUtils where

import Language.Java.Parser
import Language.Java.Syntax

import Data.Maybe


getBodyClass :: CompilationUnit -> [TypeDecl]
getBodyClass (CompilationUnit _ _ td) = td 


getClasses :: [TypeDecl] -> [ClassDecl]
getClasses = catMaybes . map fn
    where
        fn x = case x of
                ClassTypeDecl cd -> Just cd
                _                -> Nothing


getClassBody :: ClassDecl -> Maybe ClassBody
getClassBody (ClassDecl _ _ _ _ _ clsbdy) = Just clsbdy
getClassBody (EnumDecl _ _ _ _)           = Nothing


getClassName :: ClassDecl -> Maybe String
getClassName (ClassDecl _ (Ident str) _ _ _ _) = Just str
getClassName (EnumDecl _ _ _ _)                = Nothing


getDecl :: ClassBody -> [Decl]
getDecl (ClassBody ds) = ds


getD :: (MemberDecl -> a) -> Decl -> a
getD f (MemberDecl m) = f m
getD _ (InitDecl _ _) = undefined


getFieldDecl :: [Decl] -> [(Type, [VarDecl])]
getFieldDecl = catMaybes . map (getD fn)
    where
        fn :: MemberDecl -> Maybe (Type, [VarDecl])
        fn (FieldDecl _ t vl) = Just (t, vl)
        fn _ = Nothing



fixDecL :: (Type, [VarDecl]) -> [(Type, VarDecl)]
fixDecL (typ, vs) = map (\x -> (typ, x)) vs


getNameVar :: VarDecl -> String
getNameVar (VarDecl (VarId (Ident str)) _) = str

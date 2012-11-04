module TypeString
    ( typeString
    , assertionString
    , contextString
    , kindString
    , qNameString
    , nameString
    , unQualType
    ) where

import Language.Haskell.Exts.Syntax
import Data.String.Utils (join)

assertionString :: Asst -> String
assertionString (ClassA qname ts) = qNameString qname ++ " " ++ join " " (map typeString ts)

contextString :: Context -> String
contextString = join "," . map assertionString

nameString :: Name -> String
nameString (Ident s) = s
nameString (Symbol s) = s

qNameString :: QName -> String
qNameString (Qual (ModuleName m) name) = m ++ "." ++ nameString name
qNameString (UnQual name)              = nameString name

kindString :: Kind -> String
kindString KindStar       = "*"
kindString KindBang       = "!"
kindString (KindFn k1 k2) = kindString k1 ++ " -> " ++ kindString k2
kindString (KindParen k)  = "(" ++ kindString k ++ ")"
kindString (KindVar name) = nameString name

typeString :: Type -> String
typeString (TyForall _ rs t)     = contextString rs ++ " => " ++ typeString t
typeString (TyFun first rest)    = typeString first ++ " -> (" ++ typeString rest ++ ")"
typeString (TyTuple _ ts)        = "(" ++ join "," (map typeString ts) ++ ")"
typeString (TyList t)            = "[" ++ typeString t ++ "]"
typeString (TyApp t1 t2)         = typeString t1 ++ " " ++ typeString t2
typeString (TyVar s)             = nameString s
typeString (TyCon qname)         = qNameString qname
typeString (TyParen t)           = "(" ++ typeString t ++ ")"
typeString (TyInfix t1 qname t2) = undefined
typeString (TyKind t k)          = typeString t ++ " :: " ++ kindString k

unQualName :: QName -> QName
unQualName (Qual _ name) = UnQual name
unQualName qname@(UnQual _) = qname

unQualAsst :: Asst -> Asst
unQualAsst a = case a of
    ClassA qname ts    -> ClassA (unQualName qname) (map unQualType ts)
    InfixA t1 qname t2 -> InfixA (unQualType t1) (unQualName qname) (unQualType t2)
    EqualP t1 t2       -> EqualP (unQualType t1) (unQualType t2)
    IParam _ _         -> undefined

unQualType :: Type -> Type
unQualType ty = case ty of
    (TyCon qname)      -> TyCon (unQualName qname)
    (TyInfix _ _ _)    -> undefined
    (TyForall b ctx t) -> TyForall b (map unQualAsst ctx) $ unQualType t
    (TyFun t1 t2)      -> TyFun (unQualType t1) (unQualType t2)
    (TyTuple bxd ts)   -> TyTuple bxd $ map unQualType ts
    (TyList t)         -> TyList $ unQualType t
    (TyApp t1 t2)      -> TyApp (unQualType t1) (unQualType t2)
    t@(TyVar _)        -> t
    (TyParen t)        -> TyParen $ unQualType t
    (TyKind t k)       -> TyKind (unQualType t) k
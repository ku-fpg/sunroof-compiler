
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Provides template Haskell code to generate instances for JavaScript
--   object wrappers (<https://github.com/ku-fpg/sunroof-compiler/wiki/JSObject-Wrapper-Types>).
module Language.Sunroof.TH
  ( deriveJSTuple
  ) where

import Language.Haskell.TH
import Language.Sunroof.Types as SRT
import Language.Sunroof.Classes
import Language.Sunroof.JS.Object
import Language.Sunroof.JS.Bool
import Data.Boolean

-- | @derive@ derives an incomplete instance for @JSTuple@,
-- as well as completing other classes.
--
-- you write the newtype explictly, and @derive@ does the rest.
--
-- > newtype JSX o = JSX JSObject
--
-- and then the start of the JSTuple instance, and the rest gets filled in
--
-- > derive [d| instance (SunroofArgument o) => JSTuple (JSX o) where
-- >                type Internals (JSX o) = (JSString,JSNumber)
-- >        |]
--
-- generates
--
-- > instance (SunroofArgument o) => Show (JSX o) where
-- >    show (JSX o) = show o
-- >
-- > instance (SunroofArgument o) => Sunroof (JSX o) where
-- >    unbox (JSX o) = unbox o
-- >    box o = JSX (box o)
-- >
-- > instance (SunroofArgument o) => IfB (JSX o) where
-- >    ifB = jsIfB
-- >
-- > type instance BooleanOf (JSX o) = JSBool
-- >
-- > instance (SunroofArgument o) => JSTuple (JSX o) where
-- >    type instance Internals (JSX o) = (JSString, JSNumber)
-- >    match o = (o ! attr "f1", o ! attr "f2")
-- >    tuple (v1,v2) = do
-- >        o <- new "Object" ()
-- >        o # attr "f1" := v1
-- >        o # attr "f2" := v2
-- >        return (JSX o)
deriveJSTuple :: Q [Dec] -> Q [Dec]
deriveJSTuple decsQ = do
        decs <- decsQ
        fmap concat $ mapM complete decs
  where
        complete :: Dec -> Q [Dec]
        complete (InstanceD cxt' (AppT (ConT typeClass) ty) decls) = do
                -- Unused: let k decls' = InstanceD cxt' hd (decls ++ decls')
                let findClass (ConT t) = t
                    findClass (AppT t1 _) = findClass t1
                    findClass _ =  error $ "strange instance head found in derive " ++ show ty
                let tConTy = findClass ty
                -- Next, find the type instance
                let internalTy = case decls of
                        [TySynInstD tyFun [_arg] internalTy] | tyFun == ''Internals -> internalTy
                        _  -> error $ "can not find usable type instance inside JSTuple"
                let findInternalStructure (TupleT _n) ts = do
                        vs <- sequence [ newName "v" | _ <- ts ]
                        return (TupE,TupP [ VarP v | v <- vs], vs `zip` [ "f" ++ show i | (i::Int) <- [1..]])
                    findInternalStructure (AppT t1 t2) ts = findInternalStructure t1 (t2 : ts)
                    findInternalStructure (ConT v) ts = do
                            info <- reify v

                            case info of
                              TyConI (DataD [] _ _ [NormalC internalCons args] []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ "f" ++ show i | (i::Int) <- [1..]]
                                       )
                              TyConI (NewtypeD [] _ _ (NormalC internalCons args) []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ "f" ++ show i | (i::Int) <- [1..]]
                                       )
                              TyConI (DataD [] _ _ [RecC internalCons args] []) -> do
                                vs <- sequence [ newName "v" | _ <- args ]
                                return ( foldl AppE (ConE internalCons)
                                       , ConP internalCons [ VarP v | v <- vs]
                                       , vs `zip` [ nameBase x | (x,_,_) <- args ]
                                       )

                              _o -> error $ "can not find internal structure of cons " ++ show (v,ts,info)
                    findInternalStructure o ts = error $ "can not find internal structure of type " ++ show (o,ts)

                (builder :: [Exp] -> Exp,unbuilder :: Pat, vars :: [(Name,String)]) <- findInternalStructure internalTy []

                -- Now work with the tConTy, to get the tCons
                info <- reify tConTy
                let tCons = case info of
                      TyConI (NewtypeD _ _ _ (NormalC tCons [(NotStrict,ConT o)]) [])
                        | o /= ''JSObject -> error $ "not newtype of JSObject"
                        | typeClass /= ''JSTuple -> error $ "not instance of JSTuple" ++ show (tConTy,''JSTuple)
                        | otherwise -> tCons
                      _ -> error $ "strange info for newtype type " ++ show info

                o <- newName "o"
                n <- newName "n"

                return [ InstanceD cxt' (AppT (ConT ''Show) ty)
                           [ FunD 'show
                              [ Clause [ConP tCons [VarP o]]
                                         (NormalB (AppE (VarE 'show) (VarE o))) []]]
                       , InstanceD cxt' (AppT (ConT ''Sunroof) ty)
                           [ FunD 'box
                              [ Clause [VarP n] (NormalB (AppE (ConE tCons)
                                                               (AppE (VarE 'box) (VarE n)))) []]
                          , FunD 'unbox
                              [ Clause [ConP tCons [VarP o]]
                                                (NormalB (AppE (VarE 'unbox) (VarE o))) []]
                           ]
                       , InstanceD cxt' (AppT (ConT ''IfB) ty)
                              [ ValD (VarP 'ifB) (NormalB (VarE 'jsIfB)) [] ]
                       , TySynInstD ''BooleanOf [ty] (ConT ''JSBool)
                       , InstanceD cxt' (AppT (ConT ''JSTuple) ty) $ decls ++
                           [ FunD 'SRT.match
                              [Clause [VarP o] (NormalB (builder
                                  [ AppE (AppE (VarE $ mkName "!") (VarE o))
                                         (AppE (VarE 'attr) (LitE $ StringL $ s))
                                  | (_,s) <- vars ])) []]
                           , FunD 'SRT.tuple
                              [ Clause [unbuilder] (NormalB (DoE (
                                        [ BindS (VarP o) (AppE (AppE (VarE 'new) (LitE $ StringL $ "Object")) (TupE []))
                                        ] ++
                                        [ NoBindS $
                                          let assign = AppE (AppE (ConE $ mkName ":=")
                                                                  (AppE (VarE 'attr) (LitE $ StringL $ s)))
                                                            (VarE v)

                                          in  AppE (AppE (VarE $ mkName "#")
                                                         (VarE o))
                                                   (assign)
                                        | (v,s) <- vars
                                        ] ++
                                        [ NoBindS $ AppE (VarE 'return) (AppE (ConE tCons) (VarE o))
                                        ]))) []
                              ]
                           ]
                       ]
        complete _ = error "need instance declaration for derivation of JSTuple."

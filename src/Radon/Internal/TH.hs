{-# LANGUAGE TemplateHaskellQuotes #-}

module Radon.Internal.TH (genTupleInstances) where

import Control.Monad (forM)
import Data.Char (chr, ord)
import Data.Foldable (Foldable (foldl'))
import Foreign (plusPtr)
import Foreign.Ptr (Ptr)
import Language.Haskell.TH (Body (NormalB), Clause (Clause), Dec (FunD, InstanceD), DecsQ, Exp (AppE, DoE, InfixE, LitE, SigE, TupE, VarE), Lit (IntegerL), Name, Overlap (Overlappable), Pat (TupP, VarP, WildP), Quote (newName), Stmt (BindS, NoBindS), Type (AppT, ConT, TupleT, VarT), mkName)
import Radon.Internal.Math (highestPowerOfTwo)

rstorable :: Name
rstorable = mkName "RStorable"

ralignment :: Name
ralignment = mkName "ralignment"

rsizeOf :: Name
rsizeOf = mkName "rsizeOf"

rpeek :: Name
rpeek = mkName "rpeek"

rpeekByteOff :: Name
rpeekByteOff = mkName "rpeekByteOff"

rpoke :: Name
rpoke = mkName "rpoke"

rpokeByteOff :: Name
rpokeByteOff = mkName "rpokeByteOff"

rclearBytes :: Name
rclearBytes = mkName "rclearBytes"

freeable :: Name
freeable = mkName "Freeable"

freeDependents :: Name
freeDependents = mkName "freeDependents"

genTupleInstances :: Int -> DecsQ
genTupleInstances tupLength = do
  vars <- forM [ord 'a' .. ord 'a' + tupLength - 1] (\x -> VarT <$> newName [chr x])
  names <- forM [ord 'a' .. ord 'a' + tupLength - 1] (\x -> newName [chr x])
  let sizeBytes = foldl' (\acc var -> InfixE (Just acc) (VarE '(+)) (Just (VarE rsizeOf `AppE` SigE (VarE 'undefined) var))) (LitE (IntegerL 0)) vars
  return
    [ InstanceD
        (Just Overlappable)
        (map (AppT (ConT rstorable)) vars ++ map (AppT (ConT freeable)) vars)
        (ConT rstorable `AppT` (foldl' (\acc var -> acc `AppT` var) (TupleT tupLength) vars))
        [ FunD ralignment [Clause [] (NormalB (InfixE (Just (VarE 'highestPowerOfTwo)) (VarE '(.)) (Just (VarE rsizeOf)))) []],
          FunD rsizeOf [Clause [WildP] (NormalB sizeBytes) []],
          FunD
            rpeek
            [ let p = mkName "_p"
               in Clause
                    [VarP p]
                    ( NormalB
                        ( DoE
                            Nothing
                            ( snd
                                ( foldl'
                                    ( \(size, stmts) (name, var) ->
                                        ( InfixE (Just size) (VarE '(+)) (Just (VarE rsizeOf `AppE` SigE (VarE 'undefined) var)),
                                          (BindS (VarP name) (VarE rpeekByteOff `AppE` VarE p `AppE` size)) : stmts
                                        )
                                    )
                                    (LitE (IntegerL 0), [])
                                    (zip names vars)
                                )
                                ++ [NoBindS (VarE 'return `AppE` TupE (map (Just . VarE) names))]
                            )
                        )
                    )
                    []
            ],
          FunD
            rpoke
            [ let p = mkName "_p"
               in Clause
                    [VarP p, TupP (map VarP names)]
                    ( NormalB
                        ( DoE
                            Nothing
                            ( snd
                                ( foldl'
                                    ( \(size, stmts) var ->
                                        ( InfixE (Just size) (VarE '(+)) (Just (VarE rsizeOf `AppE` SigE (VarE 'undefined) var)),
                                          (NoBindS (VarE freeDependents `AppE` (SigE (VarE 'plusPtr `AppE` VarE p `AppE` size) (ConT ''Ptr `AppT` var)))) : stmts
                                        )
                                    )
                                    (LitE (IntegerL 0), [])
                                    vars
                                )
                                ++ [NoBindS (VarE rclearBytes `AppE` sizeBytes `AppE` VarE p)]
                                ++ snd
                                  ( foldl'
                                      ( \(size, stmts) (name, var) ->
                                          ( InfixE (Just size) (VarE '(+)) (Just (VarE rsizeOf `AppE` SigE (VarE 'undefined) var)),
                                            (NoBindS (VarE rpokeByteOff `AppE` VarE p `AppE` size `AppE` VarE name)) : stmts
                                          )
                                      )
                                      (LitE (IntegerL 0), [])
                                      (zip names vars)
                                  )
                            )
                        )
                    )
                    []
            ]
        ],
      InstanceD
        (Just Overlappable)
        (map (AppT (ConT rstorable)) vars ++ map (AppT (ConT freeable)) vars)
        (ConT freeable `AppT` (foldl' (\acc var -> acc `AppT` var) (TupleT tupLength) vars))
        [ FunD
            freeDependents
            [ let p = mkName "_p"
               in Clause
                    [VarP p]
                    ( NormalB
                        ( DoE
                            Nothing
                            ( snd
                                ( foldl'
                                    ( \(size, stmts) var ->
                                        ( InfixE (Just size) (VarE '(+)) (Just (VarE rsizeOf `AppE` SigE (VarE 'undefined) var)),
                                          (NoBindS (VarE freeDependents `AppE` (SigE (VarE 'plusPtr `AppE` VarE p `AppE` size) (ConT ''Ptr `AppT` var)))) : stmts
                                        )
                                    )
                                    (LitE (IntegerL 0), [])
                                    vars
                                )
                            )
                        )
                    )
                    []
            ]
        ]
    ]
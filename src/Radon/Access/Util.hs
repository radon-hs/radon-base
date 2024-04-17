{-# LANGUAGE TemplateHaskellQuotes #-}

module Radon.Access.Util (field, access) where

import Data.Foldable (Foldable (foldl'), foldlM)
import Foreign (Ptr, plusPtr)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Radon.Access
import Radon.Access.Class (rsizeOf)
import Text.Parsec (char, digit, eof, letter, many1, noneOf, parse, spaces, try, (<|>))
import Text.Parsec.String (GenParser)

data TypeExpr
  = PlainType {typeExprName :: String}
  | ArrayType {typeExprName :: String}

data AccessType
  = ARun
  | ARead
  | AWrite
  | ALRead
  | ALWrite
  | AModify
  | AModifyIO

data AccessExpr
  = FieldAccess String
  | FieldTypeAccess
  | ArrayAccessLit Int
  | ArrayAccessVar String
  | TupleAccess Int

field :: QuasiQuoter
field =
  QuasiQuoter
    { quoteExp = parseFields,
      quotePat = undefined,
      quoteDec = undefined,
      quoteType = undefined
    }

access :: QuasiQuoter
access =
  QuasiQuoter
    { quoteExp = parseAccess,
      quotePat = undefined,
      quoteDec = undefined,
      quoteType = undefined
    }

parseFields :: String -> Q Exp
parseFields s =
  case parse parserWithType "" s of
    Left err -> fail (show err)
    Right (e, name) ->
      (\(a, _, _) -> a)
        <$> foldFields
          (VarE 'idPtrAccessor, ConT (mkName (typeExprName name)), case name of PlainType _ -> False; ArrayType _ -> True)
          e

parseAccess :: String -> Q Exp
parseAccess s =
  case parse parserWithValue "" s of
    Left err -> fail (show err)
    Right (e, accessType, t, name) ->
      let n = mkName name
       in do
            (accessor, _, isResultArrayLike) <- foldFields (VarE 'idPtrAccessor, ConT (mkName (typeExprName t)), case t of { PlainType _ -> False; ArrayType _ -> True }) e
            case accessType of
              ARun -> return (VarE (if isResultArrayLike then 'runArray else 'runAccessor) `AppE` accessor `AppE` VarE n)
              ARead -> return (VarE (if isResultArrayLike then 'readArray else 'readAccessor) `AppE` accessor `AppE` VarE n)
              AWrite -> return (VarE (if isResultArrayLike then 'writeArray else 'writeAccessor) `AppE` accessor `AppE` VarE n)
              ALRead -> return (VarE 'readArrayLength `AppE` accessor `AppE` VarE n)
              ALWrite -> return (VarE 'writeArrayLength `AppE` accessor `AppE` VarE n)
              AModify -> return (VarE (if isResultArrayLike then 'modifyArray else 'modifyAccessor) `AppE` accessor `AppE` VarE n)
              AModifyIO -> return (VarE (if isResultArrayLike then 'modifyArrayIO else 'modifyAccessorIO) `AppE` accessor `AppE` VarE n)

foldFields :: (Exp, Type, Bool) -> [AccessExpr] -> Q (Exp, Type, Bool)
foldFields =
  foldlM
    ( \(acc, t, isArray) expr -> case unapply t of
        ConT tn : _ -> case expr of
          FieldAccess f -> do
            let n = mkName ("a'" ++ nameBase tn ++ "'" ++ f)
            at <- reifyType n
            case unapply at of
              xs@(x : _ : c : _) -> do
                isAccessor <- if length xs == 3 then isInstance ''Accessor [x] else return False
                isArrayLike <- if length xs == 4 then isInstance ''ArrayLike [x] else return False
                if isAccessor
                  then return (InfixE (Just acc) (VarE '(>.)) (Just (VarE n)), c, False)
                  else
                    if isArrayLike
                      then return (InfixE (Just acc) (VarE '(>>.)) (Just (VarE n)), c, True)
                      else fail (nameBase n ++ " is not an accessor!")
              _ -> fail (nameBase n ++ " is not the right type!")
          FieldTypeAccess -> do
            let n = mkName ("a'" ++ nameBase tn)
            at <- reifyType n
            case unapply at of
              xs@(x : _ : c : _) -> do
                isAccessor <- if length xs == 3 then isInstance ''Accessor [x] else return False
                if isAccessor && c == ConT ''Int
                  then return (InfixE (Just acc) (VarE '(>.)) (Just (VarE n)), c, False)
                  else fail (nameBase n ++ " is not a type accessor!")
              _ -> fail (nameBase n ++ " is not the right type!")
          ArrayAccessLit i -> do
            if isArray
              then return ((VarE 'advanceArray `AppE` LitE (IntegerL (fromIntegral i))) `AppE` acc, t, False)
              else fail "tried to use `advance` on something that is not array-like!"
          ArrayAccessVar v -> do
            if isArray
              then return ((VarE 'advanceArray `AppE` VarE (mkName v)) `AppE` acc, t, False)
              else fail "tried to use `advance` on something that is not an array-like!"
          TupleAccess _ -> fail "tried to use tuple access on something that is not a tuple!"
        TupleT _ : fs -> case expr of
          TupleAccess n -> do
            let offset = foldl' (\size var -> InfixE (Just size) (VarE '(+)) (Just (VarE 'rsizeOf `AppE` SigE (VarE 'undefined) var))) (LitE (IntegerL 0)) (take n fs)
            return (InfixE (Just acc) (VarE '(>.)) (Just (ConE 'PtrAccessor `AppE` SigE (InfixE Nothing (VarE 'plusPtr) (Just offset)) (ArrowT `AppT` (ConT ''Ptr `AppT` t) `AppT` (ConT ''Ptr `AppT` (fs !! n))))), fs !! n, False)
          _ -> fail ""
        _ -> fail (show (ppr t) ++ " does not have fields!")
    )

parserWithType :: GenParser Char st ([AccessExpr], TypeExpr)
parserWithType = do
  spaces
  name <- try arrayTypeName <|> try (PlainType <$> parenValueName) <|> (PlainType <$> valueName)
  fs <- fields
  spaces
  eof
  return (fs, name)

parserWithValue :: GenParser Char st ([AccessExpr], AccessType, TypeExpr, String)
parserWithValue = do
  spaces
  operation <- many1 letter
  let accessType =
        case operation of
          "run" -> ARun
          "read" -> ARead
          "write" -> AWrite
          "lread" -> ALRead
          "lwrite" -> ALWrite
          "modify" -> AModify
          "mio" -> AModifyIO
          _ -> error ("unknown operation: " ++ operation)
  spaces
  t <- try arrayTypeName <|> try (PlainType <$> parenValueName) <|> (PlainType <$> valueName)
  spaces
  name <- try parenValueName <|> valueName
  fs <- fields
  spaces
  eof
  return (fs, accessType, t, name)

arrayTypeName :: GenParser Char st TypeExpr
arrayTypeName = do
  _ <- char '['
  spaces
  name <- many1 (noneOf "[]")
  spaces
  _ <- char ']'
  return (ArrayType name)

valueName :: GenParser Char st String
valueName = many1 (noneOf "(). ")

parenValueName :: GenParser Char st String
parenValueName = do
  _ <- char '('
  spaces
  name <- many1 (noneOf "()")
  spaces
  _ <- char ')'
  return name

fields :: GenParser Char st [AccessExpr]
fields = many1 (try fieldAccess <|> try indexAccess <|> try tupleAccess)

fieldAccess :: GenParser Char st AccessExpr
fieldAccess = do
  spaces
  _ <- char '.'
  spaces
  fn <- many1 (noneOf ".[(")
  return (if fn == "?" then FieldTypeAccess else FieldAccess fn)

indexAccess :: GenParser Char st AccessExpr
indexAccess = do
  spaces
  _ <- char '['
  spaces
  n <- try (ArrayAccessVar <$> indexVar) <|> (ArrayAccessLit <$> indexLit)
  spaces
  _ <- char ']'
  return n

tupleAccess :: GenParser Char st AccessExpr
tupleAccess = do
  spaces
  _ <- char '('
  spaces
  n <- indexLit
  spaces
  _ <- char ')'
  return (TupleAccess n)

indexLit :: GenParser Char st Int
indexLit = read <$> many1 digit

indexVar :: GenParser Char st String
indexVar = char '$' >> many1 (noneOf " []().")

unapply :: Type -> [Type]
unapply (a `AppT` b) = unapply a ++ [b]
unapply (ForallT _ _ a) = unapply a
unapply a = [a]

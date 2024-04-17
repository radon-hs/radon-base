{-# LANGUAGE QuasiQuotes #-}

module Main where

import Foreign (Storable (..), free, calloc)
import Radon.Access.Util (access)
import Radon.Access.Defaults
import Types

main :: IO ()
main = do
  ptr <- calloc
  poke
    ptr
    ( MyStructure
        { position = Vector2 0 2,
          velocity = Vector2 0.7 1.2,
          person =
            Person
              { weight = 7.1,
                favoriteNumbers = (4, 2, 0, 1),
                carModel = Nothing,
                name = "Test"
              }
        }
    )

  v <- [access|read MyStructure ptr.velocity|]
  putStrLn ("velocity: " ++ show v)

  py <- [access|read MyStructure ptr.position.y|]
  putStrLn ("position y: " ++ show py)

  cm <- [access|read MyStructure ptr.person.carModel|]
  putStrLn ("car model: " ++ show cm)

  [access|write MyStructure ptr.person.carModel|] (Just "Some model")
  
  cm' <- [access|read MyStructure ptr.person.carModel|]
  putStrLn ("new car model: " ++ show cm')

  cmv <- [access|read MyStructure ptr.person.carModel.value|]
  putStrLn ("car model value: " ++ show cmv)
  
  n <- [access|read MyStructure ptr.person.name|]
  putStrLn ("name: " ++ n)

  [access|write MyStructure ptr.person.name|] "Test1"
  
  n' <- [access|read MyStructure ptr.person.name|]
  putStrLn ("new name: " ++ n')

  fn <- [access|read MyStructure ptr.person.favoriteNumbers|]
  putStrLn ("favorite numbers: " ++ show fn)

  tfn <- [access|read MyStructure ptr.person.favoriteNumbers(3)|]
  putStrLn ("fourth favorite number: " ++ show tfn)

  free =<< [access|run MyStructure ptr.person.favoriteNumbers|]
  free ptr

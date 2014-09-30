module Main where

import Options.Applicative
import Control.Monad (join)


import GenGetSet
import GenBussinesMethod


optionsGetSet :: Parser OptionsGetSet
optionsGetSet = OptionsGetSet 
    <$> strOption
            ( long "file"
                <> help "The Java file to add the getters and setters"
                <> short 'f'
            )

optionsGenBussinesMethod :: Parser OptionsGenBussinesMethod
optionsGenBussinesMethod = OptionsGenBussinesMethod
    <$> (strOption
            ( long "quick"
                <> help "A quick string to generate the method. For example => name;int>hello:String,world:String"
                <> short 'q'
            ))




opts :: Parser (IO ())
opts = subparser
    ( command "gen-get-set" (info (helper <*> pure genGetSet <*> optionsGetSet) 
            ( progDesc "Generate the getters and setter in the given file"
            
            ))
    <>
      command "gen-bm" (info (helper <*> pure genBussinesMethod <*> optionsGenBussinesMethod)
            ( progDesc "Generate a bussines method"

            ))
    )




main :: IO ()
main = join $ execParser opt
    where
        opt = info (helper <*> opts)
                ( fullDesc
                    <> progDesc "Generate java code"
                    <> header "java-tools - a set of tools for java"
                )

module Main where

import Options.Applicative
import Control.Monad (join)


import GenGetSet


optionsGetSet :: Parser OptionsGetSet
optionsGetSet = OptionsGetSet 
    <$> strOption
            ( long "file"
                <> help "The Java file to add the getters and setters"
                <> short 'f'
            )
                            


opts :: Parser (IO ())
opts = subparser
    ( command "gen-get-set" (info (helper <*> pure genGetSet <*> optionsGetSet) 
            ( progDesc "Generate the getters and setter in the given file"
            
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

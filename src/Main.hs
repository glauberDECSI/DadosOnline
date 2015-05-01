module Main where

import           DadosOnlineChart
import           DadosOnlineJSON

main :: IO ()
main = do
  marcas <- decodificaMarcas
  container <- criaMarcaContainer marcas
  case container of
    Nothing -> print ("Erro ao extrair container de Marcas" :: String)
    Just marcaCont -> geraFiguraBarrasQuestãoUm "Cinco marcas com mais carros" ["Carros Cadastrados"] (geraDadosQuestãoUm marcaCont)

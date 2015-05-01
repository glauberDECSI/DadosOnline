module DadosOnlineChart where

import           Control.Applicative                       ((<$>))
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy

geraFiguraBarrasQuestãoUm :: String -> [String] -> [(String, [Double])] -> IO ()
geraFiguraBarrasQuestãoUm rótuloFigura rótuloDados dados = toFile def "questão1Barras.svg" $ do
  layout_title .= rótuloFigura
  layout_title_style . font_size .= 10
  layout_x_axis . laxis_generate .= autoIndexAxis (map fst dados)
  plot $ plotBars <$> bars rótuloDados (addIndexes (map snd dados))

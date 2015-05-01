module DadosOnlineJSON where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import           Data.Text            (Text)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude

{-
  Tipos de dados para tratar dados em JSON
-}

-- | Tipo para lista de Marcas
data Marca = Marca {
  idM       :: Int,
  orderM    :: Int,
  fipeNameM :: !Text,
  nameM     :: !Text,
  keyM      :: !Text
  } deriving (Eq, Show)

instance FromJSON Marca where
  parseJSON (Object v) =
    Marca <$> v .: "id"
          <*> v .: "order"
          <*> v .: "fipe_name"
          <*> v .: "name"
          <*> v .: "key"
  parseJSON _ = mzero

instance ToJSON Marca where
  toJSON (Marca idMTJ orderMTJ fipeNameMTJ nameMTJ keyMTJ) =
    object [ "id" .= idMTJ
      , "order" .= orderMTJ
      , "fipe_name" .= fipeNameMTJ
      , "name" .= nameMTJ
      , "key" .= keyMTJ
    ]

-- | Tipo de dados para a lista de Veículos
data Veículos = Veículos {
  idV        :: !Text,
  marcaV     :: !Text,
  keyV       :: !Text,
  fipeMarcaV :: !Text,
  nameV      :: !Text,
  fipeNameV  :: !Text
} deriving (Eq, Show)

instance FromJSON Veículos
  where
  parseJSON (Object v) =
    Veículos  <$> v .: "id"
              <*> v .: "marca"
              <*> v .: "key"
              <*> v .: "fipe_marca"
              <*> v .: "name"
              <*> v .: "fipe_name"
  parseJSON _ = mzero

instance ToJSON Veículos
  where
  toJSON (Veículos idVTJ marcaVTJ keyVTJ fipeMarcaVTJ nameVTJ fipeNameVTJ) =
    object [ "id" .= idVTJ
            , "marca" .= marcaVTJ
            , "key" .= keyVTJ
            , "fipe_marca" .= fipeMarcaVTJ
            , "name" .= nameVTJ
            , "fipe_name" .= fipeNameVTJ
    ]

-- | Tipo de dados para lista Modelos de carros de uma marca.
data Modelo = Modelo {
  idMd         :: !Text,
  fipeCodigoMd :: !Text,
  marcaMd      :: !Text,
  fipeMarcaMd  :: !Text,
  nameMd       :: !Text,
  keyMd        :: !Text
} deriving (Eq, Show)

instance FromJSON Modelo
  where
  parseJSON (Object v) =
    Modelo  <$> v .: "id"
            <*> v .: "fipe_codigo"
            <*> v .: "marca"
            <*> v .: "fipe_marca"
            <*> v .: "name"
            <*> v .: "key"
  parseJSON _ = mzero

instance ToJSON Modelo
  where
  toJSON (Modelo idMdTJ marcaMdTJ keyMdTJ fipeMarcaMdTJ nameMdTJ fipeNameMdTJ) =
    object [ "id" .= idMdTJ
            , "marca" .= marcaMdTJ
            , "key" .= keyMdTJ
            , "fipe_marca" .= fipeMarcaMdTJ
            , "name" .= nameMdTJ
            , "fipe_name" .= fipeNameMdTJ
    ]

data DadosVeículo = DadosVeículo {
  marcaD      :: !Text,
  timeD       :: Int,
  idD         :: !Text,
  referenciaD :: !Text,
  fipeMarcaD  :: !Text,
  precoD      :: !Text,
  veiculoD    :: !Text,
  keyD        :: !Text,
  fipeCodigoD :: !Text,
  nameD       :: !Text
} deriving (Eq, Show)

instance FromJSON DadosVeículo
  where
  parseJSON (Object v) =
    DadosVeículo  <$> v .: "marca"
                  <*> v .: "time"
                  <*> v .: "id"
                  <*> v .: "referencia"
                  <*> v .: "fipe_marca"
                  <*> v .: "preco"
                  <*> v .: "veiculo"
                  <*> v .: "key"
                  <*> v .: "fipe_codigo"
                  <*> v .: "name"
  parseJSON _ = mzero

instance ToJSON DadosVeículo
  where
  toJSON (DadosVeículo marcaDTJ timeDTJ idDTJ referenciaDTJ fipeMarcaDTJ precoDTJ veiculoDTJ keyDTJ fipeCodigoDTJ nameDTJ) =
    object [ "marca" .= marcaDTJ
            , "time" .= timeDTJ
            , "id" .= idDTJ
            , "referencia" .= referenciaDTJ
            , "fipe_marca" .= fipeMarcaDTJ
            , "preco" .= precoDTJ
            , "veiculo" .= veiculoDTJ
            , "key" .= keyDTJ
            , "fipe_codigo" .= fipeCodigoDTJ
            , "name" .= nameDTJ
    ]


{-
  URLs para consultas
-}

-- | URL para listar as marcas disponíveis de Carros.
marcasURL :: String
marcasURL = "http://fipeapi.appspot.com/api/1/carros/marcas.json"

-- | URL para consultar carros de uma dada marca.
--   Adicionar ID_MARCA.json
-- GET: http://fipeapi.appspot.com/api/1/carros/veiculos/ID_MARCA.json
veículosURL :: String
veículosURL = "http://fipeapi.appspot.com/api/1/carros/veiculos/"

-- | URL para consultar carros de uma dada marca.
--   Adicionar ID_MARCA/ID_VEICULOS.json
-- GET: http://fipeapi.appspot.com/api/1/carros/veiculo/ID_MARCA/ID_VEICULOS.json
modeloURL :: String
modeloURL = "http://fipeapi.appspot.com/api/1/carros/veiculo/"

-- | URL para consultar os dados de um veículo.
--   Adicionar ID_MARCA/ID_VEICULOS/ID_VEICULO.json
-- GET: http://fipeapi.appspot.com/api/1/carros/veiculo/ID_MARCA/ID_VEICULOS/ID_VEICULO.json
dadosVeículoURL :: String
dadosVeículoURL = "http://fipeapi.appspot.com/api/1/carros/veiculo/"

{-
  Funções para consulta dos arquivos JSON remotos.
  TODO: Escrever em LOG o resultado das consultas.
-}

-- | Lê arquivo remoto para Marcas
marcasJSON :: IO B.ByteString
marcasJSON = simpleHttp marcasURL

-- | Lê arquivo remoto para carros de uma dada marca.
veículosJSON :: String -> IO B.ByteString
veículosJSON id_marca = simpleHttp $ veículosURL ++ id_marca ++ ".json"

-- | Lê arquivo remoto para modelos de carros de uma dada marca.
modeloJSON :: String -> String -> IO B.ByteString
modeloJSON id_marca id_veiculos = simpleHttp $ modeloURL ++ id_marca ++ "/" ++ id_veiculos ++ ".json"

-- | Lê arquivo remoto para modelos de carros de uma dada marca.
dadosVeículoJSON :: String -> String -> String -> IO B.ByteString
dadosVeículoJSON id_marca id_veiculos id_veiculo = simpleHttp $ dadosVeículoURL ++ id_marca ++ "/" ++ id_veiculos ++ "/" ++ id_veiculo ++".json"

{-
  Funções IO para decodificar os dados JSON.
  Resultados retornados em Maybe.
-}
decodificaMarcas :: IO (Maybe [Marca])
decodificaMarcas = (decode <$> marcasJSON) :: IO (Maybe [Marca])

decodificaVeículos :: String -> IO (Maybe [Veículos])
decodificaVeículos id_marca = (decode <$> veículosJSON id_marca) :: IO (Maybe [Veículos])

{-
  Container para quantidade de marcas
-}
data MarcaContainer = MarcaContainer {
  marca      :: Marca,
  quantidade :: Int
} deriving (Eq, Show)

criaMarcaContainer :: Maybe [Marca] -> IO (Maybe [MarcaContainer])
criaMarcaContainer mMarcas =
  case mMarcas of
    Nothing -> return Nothing
    Just marcas ->
      do
        mConts <- marcas2MarcaContainers marcas
        return $ Just mConts
  where
  marca2MarcaContainer :: Marca -> IO MarcaContainer
  marca2MarcaContainer m =
    do
      veículos <- decodificaVeículos (show $ idM m)
      case veículos of
        Nothing -> return MarcaContainer {marca = m, quantidade = 0}
        Just lista -> return MarcaContainer {marca = m, quantidade = length lista}

  marcas2MarcaContainerIOs :: [Marca] -> [IO MarcaContainer]
  marcas2MarcaContainerIOs = map marca2MarcaContainer

  marcas2MarcaContainers :: [Marca] -> IO [MarcaContainer]
  marcas2MarcaContainers = sequence . marcas2MarcaContainerIOs

{-
  Operações auxilixares sobre [MarcaContainer]
-}

ordenaMarcaContainers :: [MarcaContainer] -> [MarcaContainer]
ordenaMarcaContainers = sortBy ((flip . comparing) quantidade)

totalMarcasCadastradas :: [MarcaContainer] -> Int
totalMarcasCadastradas = length

totalCarrosCadastrados :: [MarcaContainer] -> Int
totalCarrosCadastrados marcasCnt = sum $ map quantidade marcasCnt

geraDadosQuestãoUm :: [MarcaContainer] -> [(String, [Double])]
geraDadosQuestãoUm marcasCnt = zip rótulosCincoMarcas carrosCincoMarcasLL

  where
    marcasCntOrdenadas = ordenaMarcaContainers marcasCnt
    totalDeCarros = fromIntegral $ totalCarrosCadastrados marcasCntOrdenadas
    cincoMarcas = take 5 marcasCntOrdenadas
    rótulosCincoMarcas = map (show . nameM . marca) cincoMarcas
    carrosCincoMarcas = map ((*100) . (/totalDeCarros) . fromIntegral . quantidade) cincoMarcas
    carrosCincoMarcasLL = map (:[]) carrosCincoMarcas

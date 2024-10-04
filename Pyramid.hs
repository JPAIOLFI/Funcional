module HighestValuePyramid (pyramid) where

import Data.List (group, sort)
import Data.Maybe (isJust, fromJust)

pyramid :: [Int] -> Maybe Int
pyramid blocos
    | length blocos < 6 = Nothing
    | otherwise = 
        let blocosOrdenados = reverse $ sort blocos  -- Ordena as pedras em ordem decrescente
            blocosAgrupados = group blocosOrdenados  -- Agrupa pedras iguais
            -- Extraindo os valores das camadas da pirâmide
            (topo, meio, base) = getValoresPiramide blocosAgrupados
        in if isJust topo && isJust meio && isJust base
           then Just $ (fromJust base) * 3 + (fromJust meio) * 2 + (fromJust topo) 
           else Nothing  

-- Função que obtém os valores das camadas da pirâmide
getValoresPiramide :: [[Int]] -> (Maybe Int, Maybe Int, Maybe Int)
getValoresPiramide [] = (Nothing, Nothing, Nothing)
getValoresPiramide grupos = (valorTopo, valorMeio, valorBase)
  where
    valorBase = getValorBase grupos
    valorMeio = getValorMeio grupos valorBase
    valorTopo = getValorTopo grupos valorBase valorMeio

-- Função para encontrar o valor da base 
getValorBase :: [[Int]] -> Maybe Int
getValorBase [] = Nothing
getValorBase (g:gs)
    | length g >= 3 = Just (head g)  
    | otherwise = getValorBase gs   

-- Função para encontrar o valor do meio
getValorMeio :: [[Int]] -> Maybe Int -> Maybe Int
getValorMeio _ Nothing = Nothing
getValorMeio grupos (Just base) = auxValorMeio grupos base

auxValorMeio :: [[Int]] -> Int -> Maybe Int
auxValorMeio [] _ = Nothing
auxValorMeio (g:gs) base
    | length g >= 2 && head g /= base = Just (head g)  
    | otherwise = auxValorMeio gs base            

-- Função para encontrar o valor do topo 
getValorTopo :: [[Int]] -> Maybe Int -> Maybe Int -> Maybe Int
getValorTopo _ Nothing _ = Nothing
getValorTopo _ _ Nothing = Nothing
getValorTopo grupos (Just base) (Just meio) = auxValorTopo grupos base meio

auxValorTopo :: [[Int]] -> Int -> Int -> Maybe Int
auxValorTopo [] _ _ = Nothing
auxValorTopo (g:gs) base meio
    | head g /= base && head g /= meio = Just (head g)  
    | otherwise = auxValorTopo gs base meio        

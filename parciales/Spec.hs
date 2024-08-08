module Spec where
  
import PdePreludat

import Library 

import Alfajores

import MinecraftParcial

import ParcialDune

import ParcialMario

import ParcialPlagio

import HeroesLeyenda

import Vacaciones

import Fmi 

import PisteoComoCampeon 

import EscuelitaThanos

import HaskellEspacial

import SamuraiJack

import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2


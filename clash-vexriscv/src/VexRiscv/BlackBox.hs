-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module VexRiscv.BlackBox where

import Prelude

import Control.Monad.State (State)
import Data.List.Infinite (Infinite(..), (...))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)
import GHC.Stack (HasCallStack)

import Clash.Backend (Backend)
import Clash.Netlist.Types (TemplateFunction(..), BlackBoxContext)

import qualified Clash.Netlist.BlackBox.Types as N
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Primitives.DSL as DSL

listToTup5 :: [a] -> (a, a, a, a, a)
listToTup5 [a, b, c, d, e] = (a, b, c, d, e)
listToTup5 _ = error "listToTup5: list must have 5 elements"

vexRiscvBBF :: HasCallStack => N.BlackBoxFunction
vexRiscvBBF _isD _primName _args _resTys = pure $ Right (bbMeta, bb)
 where
  bbMeta = N.emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes = []
      -- TODO:
      -- [ ( ("VexRiscv", "v")
      --   , BBFunction (show 'vexRiscvVerilogTF) 0 (vexRiscvVerilogTF path))
      -- ]
    }

  bb :: N.BlackBox
  bb = N.BBFunction (show 'vexRiscvTF) 0 vexRiscvTF

vexRiscvTF :: TemplateFunction
vexRiscvTF =
  let _knownDomain :< srcPath :< clk :< rst :< inp :< _ = (0...)
  in  TemplateFunction [srcPath, clk, rst, inp] (const True) vexRiscvTF#

vexRiscvTF# :: Backend backend => BlackBoxContext -> State backend Doc
vexRiscvTF# bbCtx
  | [_knownDomain, clk, rst, inp] <- map fst (DSL.tInputs bbCtx)
  , [outputTy@(N.Product _ _ [iWishboneM2Sty, dWishboneM2Sty])] <- map snd (N.bbResults bbCtx)
  , N.Product _ _ [adrTy, datMosiTy, selTy, _lockTy, cycTy, stbTy, weTy, ctiTy, bteTy] <- iWishboneM2Sty
  = do
  let
    compName :: Text
    compName = "VexRiscv"

  instName <- Id.make (compName <> "_inst")
  DSL.declarationReturn bbCtx (compName <> "_block") $ do
    ( timerInterrupt
     , externalInterrupt
     , softwareInterrupt
     , iBusWbS2M
     , dBusWbS2M
     ) <- listToTup5 <$> DSL.deconstructProduct inp ["timerInt", "extInt", "softInt", "iBusWbS2M", "dBusWbS2M"]

    (  iBusWishbone_DAT_MISO
     , iBusWishbone_ACK
     , iBusWishbone_ERR
     , _iBusWishbone_STL
     , _iBusWishbone_RTY
     ) <- listToTup5 <$> DSL.deconstructProduct iBusWbS2M ["i_rdata", "i_ack", "i_err", "i_stall", "i_retry"]

    (  dBusWishbone_DAT_MISO
     , dBusWishbone_ACK
     , dBusWishbone_ERR
     , _dBusWishbone_STL
     , _dBusWishbone_RTY
     ) <- listToTup5 <$> DSL.deconstructProduct dBusWbS2M ["d_rdata", "d_ack", "d_err", "d_stall", "d_retry"]

    iBusWishbone_CYC      <- DSL.declare "i_cyc" cycTy
    iBusWishbone_STB      <- DSL.declare "i_stb" stbTy
    iBusWishbone_WE       <- DSL.declare "i_we" weTy
    iBusWishbone_ADR      <- DSL.declare "i_adr" adrTy
    iBusWishbone_DAT_MOSI <- DSL.declare "i_dat_mosi" datMosiTy
    iBusWishbone_SEL      <- DSL.declare "i_sel" selTy
    iBusWishbone_CTI      <- DSL.declare "i_cti" ctiTy
    iBusWishbone_BTE      <- DSL.declare "i_bte" bteTy

    dBusWishbone_CYC      <- DSL.declare "d_cyc" cycTy
    dBusWishbone_STB      <- DSL.declare "d_stb" stbTy
    dBusWishbone_WE       <- DSL.declare "d_we" weTy
    dBusWishbone_ADR      <- DSL.declare "d_adr" adrTy
    dBusWishbone_DAT_MOSI <- DSL.declare "d_dat_mosi" datMosiTy
    dBusWishbone_SEL      <- DSL.declare "d_sel" selTy
    dBusWishbone_CTI      <- DSL.declare "d_cti" ctiTy
    dBusWishbone_BTE      <- DSL.declare "d_bte" bteTy

    let
      generics = []

      inps :: [(Text, DSL.TExpr)]
      inps =
        [ ("clk", clk)
        , ("reset", rst)
        , ("timerInterrupt", timerInterrupt)
        , ("externalInterrupt", externalInterrupt)
        , ("softwareInterrupt", softwareInterrupt)
        , ("iBusWishbone_DAT_MISO", iBusWishbone_DAT_MISO)
        , ("iBusWishbone_ACK", iBusWishbone_ACK)
        , ("iBusWishbone_ERR", iBusWishbone_ERR)
        , ("dBusWishbone_DAT_MISO", dBusWishbone_DAT_MISO)
        , ("dBusWishbone_ACK", dBusWishbone_ACK)
        , ("dBusWishbone_ERR", dBusWishbone_ERR)
        ]

      outs :: [(Text, DSL.TExpr)]
      outs =
        [ ("iBusWishbone_CYC",      iBusWishbone_CYC)
        , ("iBusWishbone_STB",      iBusWishbone_STB)
        , ("iBusWishbone_WE",       iBusWishbone_WE)
        , ("iBusWishbone_ADR",      iBusWishbone_ADR)
        , ("iBusWishbone_DAT_MOSI", iBusWishbone_DAT_MOSI)
        , ("iBusWishbone_SEL",      iBusWishbone_SEL)
        , ("iBusWishbone_CTI",      iBusWishbone_CTI)
        , ("iBusWishbone_BTE",      iBusWishbone_BTE)
        , ("dBusWishbone_CYC",      dBusWishbone_CYC)
        , ("dBusWishbone_STB",      dBusWishbone_STB)
        , ("dBusWishbone_WE",       dBusWishbone_WE)
        , ("dBusWishbone_ADR",      dBusWishbone_ADR)
        , ("dBusWishbone_DAT_MOSI", dBusWishbone_DAT_MOSI)
        , ("dBusWishbone_SEL",      dBusWishbone_SEL)
        , ("dBusWishbone_CTI",      dBusWishbone_CTI)
        , ("dBusWishbone_BTE",      dBusWishbone_BTE)
        ]

    DSL.instDecl N.Empty (Id.unsafeMake compName) instName generics inps outs

    pure [DSL.constructProduct outputTy
      [ DSL.constructProduct iWishboneM2Sty
        [ iBusWishbone_ADR
        , iBusWishbone_DAT_MOSI
        , iBusWishbone_SEL
        , DSL.litTExpr (DSL.B False)
        , iBusWishbone_CYC
        , iBusWishbone_STB
        , iBusWishbone_WE
        , iBusWishbone_CTI
        , iBusWishbone_BTE
        ]
      , DSL.constructProduct dWishboneM2Sty
        [ dBusWishbone_ADR
        , dBusWishbone_DAT_MOSI
        , dBusWishbone_SEL
        , DSL.litTExpr (DSL.B False)
        , dBusWishbone_CYC
        , dBusWishbone_STB
        , dBusWishbone_WE
        , dBusWishbone_CTI
        , dBusWishbone_BTE
        ]
      ]]

vexRiscvTF# bbCtx = error (ppShow bbCtx)

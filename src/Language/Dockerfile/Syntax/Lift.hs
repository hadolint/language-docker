{-# LANGUAGE TemplateHaskell #-}

module Language.Dockerfile.Syntax.Lift where

import Instances.TH.Lift
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax

import Language.Dockerfile.Syntax

deriveLift ''Ports

deriveLift ''BaseImage

deriveLift ''Instruction

deriveLift ''InstructionPos

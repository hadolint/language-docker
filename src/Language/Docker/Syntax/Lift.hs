{-# LANGUAGE TemplateHaskell #-}

module Language.Docker.Syntax.Lift where

import Instances.TH.Lift ()
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax ()

import Language.Docker.Syntax

deriveLift ''Protocol

deriveLift ''Port

deriveLift ''Ports

deriveLift ''ImageAlias

deriveLift ''BaseImage

deriveLift ''Instruction

deriveLift ''InstructionPos

deriveLift ''SourcePath
deriveLift ''TargetPath

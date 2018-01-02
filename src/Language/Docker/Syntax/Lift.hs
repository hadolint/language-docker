{-# LANGUAGE TemplateHaskell #-}

module Language.Docker.Syntax.Lift where

import Data.Fixed (Fixed)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (DiffTime)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift
import Language.Haskell.TH.Syntax ()

import Language.Docker.Syntax

deriveLift ''NonEmpty

deriveLift ''Fixed

deriveLift ''DiffTime

deriveLift ''Protocol

deriveLift ''Port

deriveLift ''Ports

deriveLift ''ImageAlias

deriveLift ''BaseImage

deriveLift ''Instruction

deriveLift ''InstructionPos

deriveLift ''SourcePath

deriveLift ''TargetPath

deriveLift ''Chown

deriveLift ''CopySource

deriveLift ''CopyArgs

deriveLift ''AddArgs

deriveLift ''Duration

deriveLift ''Retries

deriveLift ''CheckArgs

deriveLift ''Check

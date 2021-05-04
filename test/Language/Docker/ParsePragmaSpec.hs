module Language.Docker.ParsePragmaSpec where

import qualified Data.Text as Text
import Language.Docker.Parser
import Language.Docker.Syntax
import TestHelper
import Test.HUnit hiding (Label)
import Test.Hspec


spec :: Spec
spec = do
  describe "parse # pragma" $ do
    it "# escape = \\" $
      let dockerfile = Text.unlines ["# escape = \\\\"]  -- this need double escaping for some reason.
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '\\'})]
    it "#escape=`" $
      let dockerfile = Text.unlines ["#escape=`"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "# escape=`" $
      let dockerfile = Text.unlines ["# escape=`"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "#escape =`" $
      let dockerfile = Text.unlines ["#escape =`"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "#escape= `" $
      let dockerfile = Text.unlines ["#escape= `"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "# escape =`" $
      let dockerfile = Text.unlines ["# escape =`"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "#escape = `" $
      let dockerfile = Text.unlines ["#escape = `"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "# escape = `" $
      let dockerfile = Text.unlines ["# escape = `"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "# Escape = `" $
      let dockerfile = Text.unlines ["# escape = `"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "# ESCAPE = `" $
      let dockerfile = Text.unlines ["# escape = `"]
       in assertAst dockerfile [Pragma (Escape EscapeChar {escape = '`'})]
    it "#syntax=docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["#syntax=docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# syntax=docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax=docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "#syntax =docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["#syntax =docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "#syntax= docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["#syntax= docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# syntax =docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax =docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "#syntax = docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["#syntax = docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# syntax= docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax= docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# syntax = docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax = docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# Syntax = docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax = docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]
    it "# SYNTAX = docker/dockerfile:1.0" $
      let dockerfile = Text.unlines ["# syntax = docker/dockerfile:1.0"]
       in assertAst dockerfile [Pragma (Syntax SyntaxImage {syntax = "docker/dockerfile:1.0"})]

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs major mode for editing Xplain files. Xplain is a database language for data definition and data manipulation, with files typically using the `.ddl` extension.

## Architecture

**xplain-mode.el** - The single-file implementation containing:
- Font-lock keyword definitions for syntax highlighting
- Mode setup function (`xplain-mode`)
- Support for Xplain keywords (and, any, assert, base, cascade, database, etc.)
- Support for Xplain types (A, B, D, I, M, P, R, T, V with numeric suffixes)
- Comment syntax support (# as comment character)

#!/bin/bash
set -eu
cd "$(dirname "$0")"
codespell --builtin clear,rare,en-GB_to_en-US ./*.tex ../tabs/*.tex
exec latexmk -xelatex -interaction=nonstopmode -halt-on-error main.tex

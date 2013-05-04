#!/usr/bin/env bash

## The script requires pyflakes and pep8 installed
pyflakes "$1"
pep8 --ignore=E221,E701,E202 --repeat "$1"

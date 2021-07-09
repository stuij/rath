#!/bin/bash

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

echo Adding source files to ROM...

padbin 0x100 $ROOT/rath.gba
gbfs boot.gbfs $ROOT/forth/lib/*
cat $ROOT/rath.gba boot.gbfs > $ROOT/covid_adventure.gba

echo Done! [ covid_adventure.gba ] created.

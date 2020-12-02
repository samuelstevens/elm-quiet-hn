#!/bin/bash

mkdir -p debug
cp src/index.html debug/
cp src/main.css debug/
elm make src/Main.elm --debug --output=debug/main.debug.js


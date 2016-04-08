@echo off
setlocal EnableDelayedExpansion

SET "heuristics=(rand dom-first max-right-cand min-right-cand large-int-first small-int-first shrunk-most-first shrunk-least-first fail-first)"
SET "outputFolder=out"
SET "output=%outputFolder%\out.txt"

if not exist %outputFolder% mkdir %outputFolder%
del %output%

for %%i in %heuristics% do run %%i
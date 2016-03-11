@echo off
setlocal EnableDelayedExpansion

SET "heuristics=(rand dom-first max-right-cand min-right-cand large-int-first small-int-first shrunk-most-first shrunk-least-first fail-first)"

for %%i in %heuristics% do run %%i
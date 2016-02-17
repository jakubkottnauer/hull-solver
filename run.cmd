@echo off
setlocal EnableDelayedExpansion

SET "heuristicName=%1"
SET "testsFolder=tests"
SET "outputFolder=out"
SET "output=%outputFolder%\%heuristicName%_out.txt"
SET "precision=0.001"

if not exist %outputFolder% mkdir %outputFolder%

> %output% (
  for /r %%i in (%testsFolder%\*) do bin\Debug\HullSolver.exe -f %%i -h %heuristicName% -p %precision%
)
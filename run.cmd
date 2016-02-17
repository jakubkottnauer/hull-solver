@echo off
setlocal EnableDelayedExpansion

SET "testName=%1"
SET "filename=tests\%testName%"
SET "outputFolder=out"
SET "output=%outputFolder%\%testName%_out.txt"
SET "precision=0.001"

if not exist %outputFolder% mkdir %outputFolder%

bin\Debug\HullSolver.exe -f %filename% -p %precision% -h rand > %output%
bin\Debug\HullSolver.exe -f %filename% -p %precision% -h dom-first >> %output%
bin\Debug\HullSolver.exe -f %filename% -p %precision% -h max-cand >> %output%
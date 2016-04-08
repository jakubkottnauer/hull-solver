@echo off
setlocal EnableDelayedExpansion

SET "heuristicName=%1"
SET "testsFolder=tests"
SET "outputFolder=out"
SET "output=%outputFolder%\out.txt"
SET "eps=0.001"

if not exist %outputFolder% mkdir %outputFolder%

>> %output% (
  echo \begin{table}[]
  echo \centering
  echo \label{!heuristicName!}
  echo \begin{tabular}{lllll}
  echo \hline
  echo Testovací soubor ^& Poèet rozpùlení intervalu ^& Poèet zúžení intervalù ^& Èas ^&  \\ \hline

  for %%i in (%testsFolder%\*) do bin\Debug\HullSolver.exe -f %%i -h %heuristicName% -p %eps% -l

  echo \end{tabular}
  echo \caption{!heuristicName!}
  echo \end{table}
  echo.
  echo.
  echo.
)

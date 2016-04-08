@echo off
setlocal EnableDelayedExpansion

SET "heuristicName=%1"
SET "testsFolder=tests"
SET "outputFolder=out"
SET "output=%outputFolder%\out.txt"
SET "eps=0.001"

if not exist %outputFolder% mkdir %outputFolder%

>> %output% (
  echo \begin{table}[H]
  echo \centering
  echo \begin{tabular}{lrrrr}
  echo \hline
  echo problém ^& \# pùlení ^& \# zúžení ^& pomìr objemu ^& èas (s^) \\ \hline

  for %%i in (%testsFolder%\*) do bin\Debug\HullSolver.exe -f %%i -h %heuristicName% -p %eps% -l

  echo \end{tabular}
  echo \caption{Výsledky pro heuristiku \emph{!heuristicName!}}
  echo \label{!heuristicName!}
  echo \end{table}
  echo.
  echo.
  echo.
)

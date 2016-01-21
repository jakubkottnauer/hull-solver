# HullSolver

HullSolver is a simple numerical constraint satisfaction problem (NCSP) solver using hull consistency.

Sample input file with two constraints and four variables:

```
x * x = a
x + y = b
x in [1,10]
y in [0,100]
a in [16,16]
b in [10,10]
```

Only primitive constraints with addition and multiplication are supported.

# Build
## Windows
Compile with fsc.exe or use Visual Studio.

## Linux
You need Mono: `sudo apt-get install mono-complete fsharp`

Build the project with: `xbuild HullSolver.fsproj`

Run with: `mono ./bin/Debug/HullSolver.exe inputFile precision`

See more info at http://fsharp.org/use/linux/

## OSX
You need Mono: `brew install mono`

Build the project with: `xbuild HullSolver.fsproj`

Run with: `mono ./bin/Debug/HullSolver.exe inputFile precision`

See more info at http://fsharp.org/use/mac/

# Run

The program can be executed either by running the executable or from the command line with arguments `inputFile` (path to a file with the problem you want to solve) and `precision` (determines how much the algorithm will reduce the intervals).

# License
GNU GPL

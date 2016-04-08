# HullSolver

HullSolver is a simple numerical constraint satisfaction problem (NCSP) solver using hull consistency.

Sample input file with two constraints and four variables. First line lists variables whose domains we aim to narrow:

```
// This is a comment
x y a b
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

Build the project with: `xbuild`

Run with: `mono ./bin/Debug/HullSolver.exe -f <path> -p <number> -h <heuristic name>`

See more info at http://fsharp.org/use/linux/

## OSX
You need Mono: `brew install mono`

Build the project with: `xbuild`

Run with: `mono ./bin/Debug/HullSolver.exe -f <path> -p <number> -h <heuristic name>`

See more info at http://fsharp.org/use/mac/

# Usage

`HullSolver.exe -f <path> -p <number> -h <heuristic name>`
- `-f` - path to a file with the problem you want to solve
- `-p` - float size under which the domains need to get to finish the calculation - try to fiddle with this value to get good results. If unspecified, 1.0 will be used.
- `-h` - heuristic you want to use, currently supported values are `rand`, `max-cand`, `dom-first`. If unspecified, `rand` will be used.
- `-l` - output in LaTeX-friendly format.

# License
GNU GPL

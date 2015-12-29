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

Only primitive constraints with addition and multiplication are supported at the moment. 
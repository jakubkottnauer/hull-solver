# HullSolver

HullSolver is a simple numerical constraint satisfaction problem (NCSP) solver using hull consistency.

Sample input file with one constraint and three variables:

```
1
x + y = z
x in [0,20]
y in [1,10]
z in [5,5]
```

Only primitive constraints with addition and multiplication are supported at the moment. 
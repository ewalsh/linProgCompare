from pulp import LpProblem, LpMaximize, LpVariable

# initialze class
model = LpProblem("Maximize Ben Alex Keen Part 2", LpMaximize)

# Define Decision Variables
X = LpVariable('X', lowBound=0, cat="Continuous")
Y = LpVariable('Y', lowBound=2, cat="Continuous")

# Define objective
model += 4*X + 3*Y

# Define Constraints
model += 2*Y <= 25 - X
model += 4*Y >= 2*X - 8
model += Y <= 2*X - 5

model.solve()
print("X = {}".format(X.varValue))
print("Y = {}".format(Y.varValue))

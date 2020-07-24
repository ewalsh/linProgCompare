from pulp import LpProblem, LpMaximize, LpVariable, value

# initialze class
model = LpProblem("Maximize Luxury Car Profit", LpMaximize)

# Define Decision Variables
A = LpVariable('A', lowBound=0, cat="Integer")
B = LpVariable('B', lowBound=0, cat="Integer")

# Define Objective
model += 30000*A + 45000*B

# Define Constraints
model += 3*A + 4*B <= 30
model += 5*A + 6*B <= 60
model += 1.5*A + 3*B <= 21

model.solve()
print("Produce {} of Car A".format(A.varValue))
print("Produce {} of Car B".format(B.varValue))
print(value(model.objective))

from pulp import LpProblem, LpVariable, LpMaximize

# initialze class
model = LpProblem("Maximize Bakery Profits", LpMaximize)

# Define Decision Variables
A = LpVariable('A', lowBound=0, cat="Integer")
B = LpVariable('B', lowBound=0, cat="Integer")

# Define Objective Function
model += 20*A + 40*B

# Define Constraints
model += 0.5*A + B <= 30
model += A + 2.5*B <= 60
model += A + 2*B <= 22

# print(model)
model.solve()
print("Produce {} Cake A".format(A.varValue))
print("Produce {} Cake B".format(B.varValue))

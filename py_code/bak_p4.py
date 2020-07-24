from pulp import LpProblem, LpMinimize, LpVariable, lpSum, \
    LpStatus, value as mod_val

# initialze class
model = LpProblem("Cost minimising blending problem", LpMinimize)

# construct decision variables
sausage_types = ['economy', 'premium']
ingredients = ['pork', 'wheat', 'starch']


def var_names(type1, group1):
    out = {}
    for typ in type1:
        for grp in group1:
            tmp = typ[0]+grp[0]
            out.update({(typ, grp): tmp})
    return out


short_names = var_names(sausage_types, ingredients)
# print(short_names)
# print(nm for nm in short_names)
# print((i, j) for i in sausage_types for j in ingredients)

# Define Decision Variables
ing_weight = LpVariable.dicts("weight kg", \
    ((i, j) for i in sausage_types for j in ingredients), \
    lowBound=0, cat='Continuous')

# print(ing_weight)

# Define Objective
model += (lpSum([4.32*ing_weight[(i, 'pork')] + \
    2.46*ing_weight[(i, 'wheat')] + \
    1.86*ing_weight[(i, 'starch')] \
    for i in sausage_types]))

print(model)

# # Define Constraints
# 350 econ and 500 prem @ 0.05kg
model += lpSum([ing_weight['economy', j] for j in ingredients]) == 350*0.05
model += lpSum([ing_weight['premium', j] for j in ingredients]) == 500*0.05
# economy has >= 40% pork, prem >= 60%
model += ing_weight['economy', 'pork'] >= ( \
    0.4*lpSum([ing_weight['economy', j] for j in ingredients]))
model += ing_weight['premium', 'pork'] >= ( \
    0.6*lpSum([ing_weight['premium', j] for j in ingredients]))
# sausages must be <= 25% starch
model += ing_weight['economy', 'starch'] <= ( \
    0.25*lpSum([ing_weight['economy', j] for j in ingredients]))
model += ing_weight['premium', 'starch'] <= ( \
    0.25*lpSum([ing_weight['premium', j] for j in ingredients]))
# can't use more than available resource
model += lpSum([ing_weight[i, 'pork'] for i in sausage_types]) <= 30
model += lpSum([ing_weight[i, 'wheat'] for i in sausage_types]) <= 20
model += lpSum([ing_weight[i, 'starch'] for i in sausage_types]) <= 17
# use at least 23kg of pork
model += lpSum([ing_weight[i, 'pork'] for i in sausage_types]) >= 23

model.solve()
LpStatus[model.status]

for var in ing_weight:
    var_value = ing_weight[var].varValue
    print("The weight of {0} in {1} sausages is {2} kg".format(var[1], \
        var[0], var_value))


total_cost = mod_val(model.objective)

print("The total cost is ${} for 350 economy sausages and 500 premium sausages".format(round(total_cost, 2)))

from pulp import LpProblem, LpMinimize  # #, LpVariable, lpSum
import pandas as pd

# Initialze the model
model = LpProblem("Minimize Transportation Cost", LpMinimize)

# Build the lists and the demand directory
warehouses = ["New York", "Atlanta"]
customers = ["East", "South", "Midwest", "West"]
regional_demand = [1800, 1200, 1100, 1000]
demand = dict(zip(customers, regional_demand))
info = {'Customer': customers, \
'New York': [211, 232, 240, 300], \
'Atlanta': [232, 212, 230, 280]}

info = pd.DataFrame(info)
info.set_index('Customer', inplace=True)


def make_dict(warehouses, customers):
    var_dict = {}
    for wh in warehouses:
        for cust in customers:
            short_name = wh[:2] + cust[:2]
            var_dict.update({(wh, cust): short_name.lower()})
    return var_dict


def make_costs(warehouses, customers, info):
    costs = {}
    for wh in warehouses:
        for cust in customers:
            costs.update({(wh, cust): info[wh].loc[cust]})
    return costs


var_dict = make_dict(warehouses, customers)
costs = make_costs(warehouses, customers, info)
print(var_dict[('New York', 'East')])
print(costs)

# Define objective
# model += lpSum([costs[(w, c)] * \
# var_dict[(w, c)] for c in customers for w in warehouses])
#
# print(model)

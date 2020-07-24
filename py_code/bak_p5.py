from pulp import LpProblem, LpMinimize, LpVariable, lpSum, \
    LpStatus, value as mod_val
import pandas as pd

# get Data
factories = pd.read_csv('./data/fac_vars.csv').set_index(['Month', 'Factory'])
# print(factories)
demand = pd.read_csv('./data/demand.csv').set_index(['Month'])
# print(demand)
# print(factories.index)

# # create variables
# first integer vars
production = LpVariable.dicts("production", \
    ((month, factory) for month, factory in factories.index), \
    lowBound=0, cat='Integer')
# binary vars
factory_status = LpVariable.dicts("factory_status", \
    ((month, factory) for month, factory in factories.index), \
    cat='Binary')
# print(factory_status)

# initialze class
model = LpProblem("Cost minimising scheduling problem", LpMinimize)

# Define Objective
model += lpSum( \
    [production[month, factory]*factories.loc[(month, factory), 'Variable_Costs'] \
    for month, factory in factories.index] \
    + [factory_status[month, factory]*factories.loc[(month, factory), 'Fixed_Costs'] \
    for month, factory in factories.index] \
    )

# print(model)

# # Define Constraints
months = demand.index
for month in months:
    model += production[(month, 'A')] + production[(month, 'B')] == demand.loc[month, 'Demand']


# set bound contraints for production capacity
for month, factory in factories.index:
    min_production = factories.loc[(month, factory), 'Min_Capacity']
    max_production = factories.loc[(month, factory), 'Max_Capacity']
    model += production[(month, factory)] >= min_production * factory_status[month, factory]
    model += production[(month, factory)] <= max_production * factory_status[month, factory]


# set production shutdown for May in factory B
model += factory_status[5, 'B'] == 0
model += production[5, 'B'] == 0
model.solve()
print(LpStatus[model.status])

output = []
for month, factory in production:
    # print((month, factory))
    var_output = {
        'Month': month, 'Factory': factory, \
        'Production': production[(month, factory)].varValue, \
        'Factory Status': factory_status[(month, factory)].varValue
    }
    # print(var_output)
    output.append(var_output)

output_df = pd.DataFrame.from_records(output).sort_values(['Month', 'Factory'])
output_df.set_index(['Month', 'Factory'], inplace=True)
print(output_df)
print(mod_val(model.objective))

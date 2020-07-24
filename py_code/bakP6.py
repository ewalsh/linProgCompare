import pandas as pd
from pulp import LpVariable, LpProblem, LpMinimize, lpSum, LpStatus, value as mod_val

# # adding startup cost to p5
factories = pd.read_csv('./data/fac_vars.csv').set_index(['Month', 'Factory'])
# print(factories)

demand = pd.read_csv('./data/demand.csv').set_index(['Month'])
# print(demand)

# create variables
production = LpVariable.dicts("production",
    ((month, factory) for month, factory in factories.index),
    lowBound=0,
    cat='Integer')

factory_status = LpVariable.dicts("factory_status",
    ((month, factory) for month, factory in factories.index),
    cat='Binary')

switch_on = LpVariable.dicts("switch_on",
    ((month, factory) for month, factory in factories.index),
    cat="Binary")

# initialize model
model = LpProblem("Cost minimizing scheduling problem", LpMinimize)

factory_A_index = [tpl for tpl in factories.index if tpl[1] == 'A']
factory_B_index = [tpl for tpl in factories.index if tpl[1] == 'B']

model += lpSum(
    [production[m, f] * factories.loc[(m, f), 'Variable_Costs'] for m, f in factories.index]
    + [factory_status[m, f] * factories.loc[(m, f), 'Fixed_Costs'] for m, f in factories.index]
    + [switch_on[m, f]*20000 for m, f in factory_A_index]
    + [switch_on[m, f]*400000 for m, f in factory_B_index]
)

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

# add constraint for switching on factory
for month, factory in factories.index:
    if month ==1:
        model += switch_on[month, factory] == factory_status[month, factory]
    else:
        model += switch_on[month, factory] >= factory_status[month, factory] - \
            factory_status[month-1, factory]
        model += switch_on[month, factory] <= 1 - factory_status[month-1, factory]
        model += switch_on[month, factory] <= factory_status[month, factory]

model.solve()
print(LpStatus[model.status])

output = []
for month, factory in production:
    # print((month, factory))
    var_output = {
        'Month': month, 'Factory': factory,
        'Production': production[(month, factory)].varValue,
        'Factory Status': factory_status[(month, factory)].varValue,
        'Switch On': switch_on[(month, factory)].varValue
    }
    # print(var_output)
    output.append(var_output)

output_df = pd.DataFrame.from_records(output).sort_values(['Month', 'Factory'])
output_df.set_index(['Month', 'Factory'], inplace=True)
print(output_df)
print(mod_val(model.objective))

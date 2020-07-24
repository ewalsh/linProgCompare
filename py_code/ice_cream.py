# from pulp import LpVariable, LpMaximize, LpProblem, lpSum
#
# # initialze
# model = LpProblem("Maximize Ice Cream Profits", LpMaximize)
#
# # create inputs
# prod_types = ["budget", "premium"]
# ingredients = ["cream", "milk", "sugar"]
#
#
# def make_dict(prod_types, ingredients):
#     var_dict = {}
#     for pt in prod_types:
#         for i in ingredients:
#             short_name = i[0] + pt[0]
#             var_dict.update({(pt, i): short_name})
#     return var_dict
#
#
# # print(make_dict(prod_types, ingredients))
# var_dict = make_dict(prod_types, ingredients)
# model += lpSum([1.5*var_dict[(i, "cream")] + \
# 0.125 * var_dict[(i, "milk")] +\
# 0.1 * var_dict[(i, "sugar")] for i in prod_types])
#
# print(model)

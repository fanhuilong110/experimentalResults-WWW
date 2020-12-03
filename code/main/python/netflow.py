#!/usr/bin/env python3.7

# Copyright 2020, Gurobi Optimization, LLC

# Solve a multi-commodity flow problem.  Two products ('Pencils' and 'Pens')
# are produced in 2 cities ('Detroit' and 'Denver') and must be sent to
# warehouses in 3 cities ('Boston', 'New York', and 'Seattle') to
# satisfy demand ('inflow[h,i]').
#
# Flows on the transportation network must respect arc capacity constraints
# ('capacity[i,j]'). The objective is to minimize the sum of the arc
# transportation costs ('cost[i,j]').

import gurobipy as gp
from gurobipy import GRB

# Base data
commodities = ['k1', 'k2']
nodes = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

arcs, capacity = gp.multidict({
    ('1', '4'):   10,
    ('1', '3'):  3,
    ('2', '4'):  6,
    ('2',  '3'):   8,
    ('3',  '5'): 7,
    ('3',  '6'):  2,
    ('4', '5'):   5,
    ('4', '7'):  6,
    ('5', '8'):  12,
    ('6',  '8'):   6,
    ('7',  '9'): 8,
    ('8',  '9'):  10,
})

# Cost for triplets commodity-source-destination
cost = {
    ('k1', '1', '3'): 2,
    ('k1', '1', '4'): 6,
    ('k1', '2', '3'): 4,
    ('k1', '2', '4'): 2,
    ('k1', '3', '5'): 8,
    ('k1', '3', '6'): 1,
    ('k1', '4', '5'): 3,
    ('k1', '4', '7'): 10,
    ('k1', '5', '8'): 2,
    ('k1', '6', '8'): 5,
    ('k1', '7', '9'): 9,
    ('k1', '8', '9'): 1,
    ('k2', '1', '3'): 7,
    ('k2', '1', '4'): 5,
    ('k2', '2', '3'): 10,
    ('k2', '2', '4'): 1,
    ('k2', '3', '5'): 5,
    ('k2', '3', '6'): 4,
    ('k2', '4', '5'): 2,
    ('k2', '4', '7'): 12,
    ('k2', '5', '8'): 4,
    ('k2', '6', '8'): 3,
    ('k2', '7', '9'): 6,
    ('k2', '8', '9'): 2
}

# Demand for pairs of commodity-city
inflow = {
    ('k1', '1'):  2,
    ('k1', '2'): 4,
    ('k1', '3'): 0,
    ('k1', '4'): 0,
    ('k1', '5'): 0,
    ('k1', '6'): 0,
    ('k1', '7'): -4,
    ('k1', '8'): -1,
    ('k1', '9'): -1,
    ('k2', '1'): 3,
    ('k2', '2'): 3,
    ('k2', '3'): 0,
    ('k2', '4'): 0,
    ('k2', '5'): 0,
    ('k2', '6'): 0,
    ('k2', '7'): -1,
    ('k2', '8'): -4,
    ('k2', '9'):  -1
}

# Create optimization model
m = gp.Model('netflow')


# Create variables
flow = m.addVars(commodities, arcs, obj=cost, name="flow")

# Arc-capacity constraints
m.addConstrs(
    (flow.sum('*', i, j) <= capacity[i, j] for i, j in arcs), "cap")

# Equivalent version using Python looping
# for i, j in arcs:
#   m.addConstr(sum(flow[h, i, j] for h in commodities) <= capacity[i, j],
#               "cap[%s, %s]" % (i, j))


# Flow-conservation constraints
m.addConstrs(
    (flow.sum(h, '*', j) + inflow[h, j] == flow.sum(h, j, '*')
        for h in commodities for j in nodes), "node")

# Alternate version:
# m.addConstrs(
#   (gp.quicksum(flow[h, i, j] for i, j in arcs.select('*', j)) + inflow[h, j] ==
#     gp.quicksum(flow[h, j, k] for j, k in arcs.select(j, '*'))
#     for h in commodities for j in nodes), "node")

# Compute optimal solution
m.optimize()


# Print solution
if m.status == GRB.OPTIMAL:
    solution = m.getAttr('x', flow)
    for h in commodities:
        print('\nOptimal flows for %s:' % h)
        costsum= 0
        for i, j in arcs:
            if solution[h, i, j] > 0:
                costsum = costsum + cost[h,i,j]*solution[h, i, j]
                print('%s -> %s: %g' % (i, j, solution[h, i, j]))
        print(costsum)

import numpy as np

class SphereProblem(ProblemSetting):
    """
    Sphere function with C1 constraint.
    fitness: f(x) = sum(x_i^2)
    constraint: sum(45 - x_i) <= 0  (i.e., sum(x_i) >= 45*D)
    """
    
    def __init__(self, dim, constraint_handling='rejection', penalty_coef=1e6):
        super().__init__(constraint_handling, penalty_coef)
        self.dim = dim
    
    def fitness(self, x):
        return sum(xi ** 2 for xi in x)
    
    def constraint(self, x):
        return sum(45 - xi for xi in x)

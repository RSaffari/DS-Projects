###Final Project-----------------------
library("lpSolve")

#Objective function
# Variables are x11, x12, ... , x67 , x77

#Cost Matrix
M = 1000000
cost = matrix(c( M , 5 , 3 , 5 , 5 , 20, 20,
                 9 , M , 9 , 1 , 1 , 8 , 15,
                0.4, 8 , M , 1 ,0.5, 10, 12,
                 M , M , M , M ,1.2, 2 , 12,
                 M , M , M ,0.8, M , 2 , 12,
                 M , M , M , M , M , M , 1 ,
                 M , M , M , M , M , 7 , M ),
              nrow = 7, ncol = 7, byrow = T)
objective_coef <- as.vector(t(cost))
objective_coef

#Constraint martix

n0 <- c(rep(0,7))
n1 <- c(rep(0,0), -1, rep(0,6))
n2 <- c(rep(0,1), -1, rep(0,5))
n3 <- c(rep(0,2), -1, rep(0,4))
n4 <- c(rep(0,3), -1, rep(0,3))
n5 <- c(rep(0,4), -1, rep(0,2))
n6 <- c(rep(0,5), -1, rep(0,1))
n7 <- c(rep(0,6), -1, rep(0,0))

# Sources constraints
c1 <- c(c(0, rep(1,6)), rep(n1,2), rep(n0,4))
c2 <- c(n2 , c(1, 0, rep(1,5)), n2, rep(n0,4))
c3 <- c(rep(n3,2), c(1, 1, 0, 1, 1, 1, 1), rep(n0,4))
# Transit constraints
c4 <- -c(rep(n4,3), c(rep(0,4), rep(1,3)), n4, rep(n0,2))
c5 <- -c(rep(n5,4), c(rep(0,3), 1, 0, rep(1,2)), rep(n0,2))
# Destination constraints
c6 <- -c(rep(n6,5), c(rep(0,6), 1), n6)
c7 <- -c(rep(n7,6), c(rep(0,5), 1, 0))
# Making constraints string
const1 <- rbind(c1,c2,c3,c4,c5,c6,c7)

const2 <- diag(1, 49)

const_mat <- rbind(const1, const2)
dim(const_mat)
View(const_mat)

#RHS for the constraints
rhs1 <- c(200, 300, 100, 0, 0, 400, 180)
rhs2 <- c(rep(200, 49))
const_rhs <- c(rhs1, rhs2)

#Constraints direction
const_dir  <- c(rep("<=", 3),  rep("==", 2), rep("=", 2), rep("<=", 49))

#Find the optimal solution
sol <-  lp(direction = "min",  
           objective.in = objective_coef, 
           const.mat = const_mat, 
           const.dir = const_dir,  
           const.rhs = const_rhs,
           compute.sens = TRUE)

sol$solution
#x11 - x17: 0   0 180   0   0   0   0   
#x21 - x27: 0   0   0 120   0 180   0   
#x31 - x37: 0   0   0  80 200   0   0   
#x41 - x47: 0   0   0   0   0 200   0
#x51 - x57: 0   0   0   0   0 200   0   
#x61 - x67: 0   0   0   0   0   0 180   
#x71 - x77: 0   0   0   0   0   0   0

matrix(sol$solution, nrow = 7, ncol = 7, byrow = TRUE)
sol$objval
#3269

#Sensitivity analysis
matrix(sol$sens.coef.from, nrow = 7, ncol = 7, byrow = TRUE)
#  0    3 0  4.0  3.5  11       12.0
# -3    0 0 -1.0  0.5   3        9.0
# -3    0 0  0.5  0.2   8        9.0
#  0    0 0  0.0 -0.5 -1.0e+30   8.0
#  0    0 0  0.5  0.0 -1.0e+30   8.5
#  0    0 0  0.0  0.0   0       -7.0
#  0    0 0  0.0  0.0  -1        0.0

matrix(sol$sens.coef.to, nrow = 7, ncol = 7, byrow = TRUE)
# N = 1e+30
# N N 4  N   N  N   N
# N N N 1.5  N  10  N
# N N N 1.3  1  N   N
# N N N  N   N  7   N
# N N N  N   N  7.5 N
# N N N  N   N  N   4
# N N N  N   N  N   N

# Plotting the result network
edgelist <- data.frame(
  from = c(1, 2, 2, 3, 3, 4, 5, 6),
  to =   c(3, 4, 6, 4, 5, 6, 6, 7),
  ID = seq(1, 8, 1),
  capacity = c(180, 120, 180, 80, 200, 200, 200, 180),
  cost = c(3, 1, 8, 1, 0.5, 2, 2, 1))


library(igraph)

# make edgelist into graph object
g <- graph_from_edgelist(as.matrix(edgelist[,c('from','to')]))

# add properties
E(g)$capacity <- edgelist$capacity
E(g)$cost <- edgelist$cost

plot(g, edge.label = E(g)$capacity)
plot(g, edge.label = E(g)$cost)

# area of intereection over union


#####
##### FEATURE 1 
#####


# ESTE CODIGO HACE ALGO NUEVO 


fun1 <- function(x){
  for (i in vector) {
    y <- do(x)
  }
  return(y)
}



#####
##### 
#####






#####
##### MOD 1 
#####



library(spMM)




#####
##### 
#####


#####
##### MOD 2
#####



library(terra)




#####
##### 
#####


#####
##### MOD 3
#####



library(sf)




#####
##### 
#####

#####
##### MOD 4
#####



library(ggmap)




#####
##### 
#####


#####
##### MOD 5
#####
<<<<<<< HEAD
=======



library(spline)




#####
##### 
#####


#####
##### MOD 6
#####



library(tidyterra)




#####
##### 
#####
>>>>>>> branch_2



library(spline)


#####
##### 
#####


# Load the required library
library(raster)


bbox_extent <- extent(c(0,  5000,  0, 5000))



resolution_km <- 1  # km
resolution_meters <- resolution_km * 1000  # convert to meters

num_rows <- ceiling((bbox_extent@ymax - bbox_extent@ymin) / resolution_meters)
num_cols <- ceiling((bbox_extent@xmax - bbox_extent@xmin) / resolution_meters)

raster_1 <- raster(xmn = bbox_extent@xmin, xmx = bbox_extent@xmax,
                   ymn = bbox_extent@ymin, ymx = bbox_extent@ymax,
                   nrows = num_rows, ncols = num_cols)
raster_2 <- raster(xmn = bbox_extent@xmin, xmx = bbox_extent@xmax,
                   ymn = bbox_extent@ymin, ymx = bbox_extent@ymax,
                   nrows = num_rows, ncols = num_cols)

projection(raster_1) <- CRS("+init=EPSG:4326")
projection(raster_2) <- CRS("+init=EPSG:4326")

res(raster_1) <- c(resolution_meters, resolution_meters)
res(raster_2) <- c(resolution_meters, resolution_meters)








####

library(MASS)

rSample <- function(nb,rho,sigma2_u,resid,intercept,slope,pairs=TRUE) {
  ## sample pairs of adjacent locations
  if (pairs) {
    x <- rnorm(nb/2); x <- c(x,x+0.001)
    y <- rnorm(nb/2); y <- c(y,y+0.001)
  } else {x <- rnorm(nb);y <- rnorm(nb)}
  dist <- dist(cbind(x,y)) ## distance matrix between locations
  m <- exp(-rho*as.matrix(dist)) ## correlation matrix
  b <- mvrnorm(1,rep(0,nb),m*sigma2_u) ## correlated random effects
  pred <- sample(nb) ## some predictor variable
  obs <- intercept+slope*pred + b +rnorm(nb,0,sqrt(resid)) ## response
  data.frame(obs=obs,x,y,pred=pred)
}

set.seed(123)
d1 <- rSample(nb=500,rho=10,sigma2_u=0.5,resid=0.5,intercept=-1,slope=0.1)
# set.seed(1234)
# d2 <- rSample(nb=40,rho=3,sigma2_u=0.5,resid=0.5,intercept=-1,slope=0.1)


# GAUSSIAN SIMULATION



coordinates(d1) = ~x+y

v<-variogram(obs~ pred, data = d1, cloud=F)
m<-vgm(1.5,"Exp",40000,0.5)

m.f<-fit.variogram(v, m)
m.f



##################
##################
##################



library(mvtnorm)   # to draw multivariate normal outcomes
library(R2jags)    # JAGS-R interface

# function that makes distance matrix for a side*side 2D array  

dist.matrix <- function(side)
{
  row.coords <- rep(1:side, times=side)
  col.coords <- rep(1:side, each=side)
  row.col <- data.frame(row.coords, col.coords)
  D <- dist(row.col, method="euclidean", diag=TRUE, upper=TRUE)
  D <- as.matrix(D)
  return(D)
}

# function that simulates the autocorrelated 2D array with a given side,
# and with exponential decay given by lambda
# (the mean mu is constant over the array, it equals to global.mu)
cor.surface <- function(side, global.mu, lambda)
{
  D <- dist.matrix(side)
  # scaling the distance matrix by the exponential decay
  SIGMA <- exp(-lambda*D)
  mu <- rep(global.mu, times=side*side)
  # sampling from the multivariate normal distribution
  M <- matrix(nrow=side, ncol=side)
  M[] <- rmvnorm(1, mu, SIGMA)
  return(M)
}

# parameters (the truth) that I will want to recover by JAGS
side = 100
global.mu = 1
lambda = 0.4  # let's try something new

# simulating the main raster that I will analyze as data
M <- cor.surface(side = side, lambda = lambda, global.mu = global.mu)


apply_mask <- function(matrix_to_mask) {
  new_matrix <- matrix_to_mask   # Create a copy of the original matrix
  new_matrix[new_matrix < 0.0] <- 1   # Set values < 0.5 to 1
  new_matrix[new_matrix >= -0.1] <- 0  # Set values >= 0.5 to 0
  return(new_matrix)
}


M_ <- apply_mask(M)



image(M_)

mean(M)




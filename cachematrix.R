## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#example given in notes
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#the inverse matrix A???1 is a matrix that when multiplied by A yields the Identity matrix of the vector space
#To find the inverse of a 2x2 matrix: swap the positions of a and d, put negatives in front of b and c
#Easiest way to think about this is to take a 2x2 matrix first
#and divide everything by the determinant (ad-bc).
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

#original given However, it first checks to see if the mean has already been calculated.
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


#Create matrix using the function
x<-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
# returns the function details
x

#returns the matrix as expected
x$get() 

#gives the inverse of the matrix, i.e. 1/value n the matrix
#is recursive = True means you can invert it in the first place
is.recursive(x)
cacheSolve(x)

#create own matrix called test
test <- matrix(c(1,2,3,4),2,2)
test
#as.data.frame(test)
#test <- list(1:9)
is.recursive(test) # for some reason is false
cacheSolve(test) # doesn't work as invalid for vectors, not seeing it as a matrix?
#Need to put it through the cachematrix function first
test2 <- makeCacheMatrix(test)
is.recursive(test2) # now true
cacheSolve(test2) # takesmaybe 5-10 secs
#run it again
cacheSolve(test2) # takes less than a second and states 'getting cached data'


#Illustration of how this working:
# a b
# c d
# 
# inverted becomes
# 
# (1/ad-bc) * 
# d -b
# -c a
# 
# 1 3
# 2 4
# 
# becomes
# 
# 4 -3
# -2 1
# 
# ad-bc = 1*4 - 3*2 = -2
# 
# so cacheSolve(test2) above gives us
# 
# 4/-2   -3/-2
# -2/-2  1/-2
# 
# -2 1.5
# 1  -0.5

#Doesn't work always
#for example
#matrix(c(1,2,3,4,5,6,7,8,9),3,3)
#because the division is by zero
#Here you have to cross out the first row and column and end up with a 2x2 matrix
# 5 8
# 6 9
# Determinant part 1 is 
# 5*9 - 8*6 = -3*1 = -3
# Next cross out first row and second columnmean
# 2 8
# 3 9
# Determinant part 2 is 
# 2*9 - 8*3 = -6*4=-24*-1 = 24 (-1 is because we always * by 1 or -1 depending on matrix position)
# Third part is formed from crossing out first rwo and last column
# 2 5
# 3 6
# Determinant part 3 is
# 2*6 - 5*3 = -3*7=-21
# Then add them up so
# -3 + 24 + -21 = 0
#Such a matrix is called "Singular", which only happens when the determinant is zero.

# Think of as algebra with a matrix
# e.g A group took a trip on a bus, at £3 per child and £3.20 per adult for a total of £118.4.
# They took the train back at £3.50 per child and £3.60 per adult for a total of £135.20.
# How many adults and kids?

# 3    3.5 = 118.4  135.2
# 3.2  3.6
#   
#   
# Determinant is
# 3*3.6 - 3.5*3.2 = -0.4
# Inverse is
# -9  8.75
# 8   -7.5
# 
# Then multiply this by resulting matrix to get the answer
# 118.4*-9 + 135.2*8     118.4*8.75 + 135.2*-7.5
# 16  22
# 16 adults
# 22 kids

  
  

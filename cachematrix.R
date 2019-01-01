# This assignment finds the inverse of a matrix either by calculating 
# the inverse, or if the inverse has been calculated previously, from a cache.
# The asignment leverages two functions -  "makeCacheMatrix" and "cacheSolve" - 
# and takes advantage of lexical scope and caching to trade memory 
# use for computation.

# The first function, "makeCacheMatrix" creates creates a special "matrix", 
# which is really a list containing a function to
#  
# 1. set the matrix, 
# 2. get the matrix, 
# 3. setinverse, which sets the inverse of the matrix, and
# 4. getinverse, which gets the inverse of the matrix

# Sample input: x<-matrix(rnorm(16),4,4)
# Cache the matrix using xM<-makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   y <- NULL
   set <- function(y) {
           x <<- y
           m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse) 
}


# The second function, "cacheSolve," returns the inverse of the matrix
# by calculating it or extracting it from the cache. 
# Sample input: xM<-makeCacheMatrix(x)
# Marix inverse: cacheSolve(xM)

cacheSolve <- function(xM, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- xM$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- xM$get()
    m <- solve(data, ...)
    xM$setinverse(m)
    m
}

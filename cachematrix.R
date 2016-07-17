# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it repeatedly. The following functions
# helps us to accomplish this task. There is a running example in the comments after the implementations.


# This function creates a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse matrix
# 4) get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse_matrix) m <<- inverse_matrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function returns the inverse of the matrix. First, it checks whether it has been compute before.
# If it has been computed before, it just returns the cached value; otherwise,
# it computes it using the "solve" function and saves it for future and returns the computed inverse matrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}

# Here is an example. As you see below, first time it directly calls the solve method but after that it only uses the
# cached value 
#> x = rbind(c(1, 2), c(3, 2))
#> t = makeCacheMatrix(x)
#> cacheSolve(t)
#      [,1]  [,2]
#[1,] -0.50  0.50
#[2,]  0.75 -0.25
#> cacheSolve(t)
#getting cached data
#      [,1]  [,2]
#[1,] -0.50  0.50
#[2,]  0.75 -0.25

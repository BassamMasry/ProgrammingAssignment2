# creates a special "vector", which is really a list containing a function to
# set the value of the matrix, get the value of the matrix,
# set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The following function calculates the inverse of the special "vector" created
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the data and sets the
# value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {    inv <- x$getinverse()
if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

# change this for testing various matrices
test_matrix <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
# make matrix test_matrix = [1 , 3
#                            2, 4]
tester <- makeCacheMatrix(test_matrix)
cacheSolve(tester)

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) inverse_matrix <<- solve
    get_inverse <- function() inverse_matrix
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}

# Example of how to use functions
matrix<-matrix(1:4,nrow=2,ncol=2)
m<-makeCacheMatrix(matrix)
# At this point, cacheSolve will find the inverse
cacheSolve(m)
# Now it should just return the cached value
cacheSolve(m)
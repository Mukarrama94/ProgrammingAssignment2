## This project implements a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invrse <- NULL
    
    set <- function(y) {
        x <<- y
        invrse <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(given_inversed_value) {
        invrse <<- given_inversed_value
    }
    
    getinverse <- function() {
        invrse
    }
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inverse_value_in_the_matrix <- x$getinverse()
    if(!is.null(inverse_value_in_the_matrix)) {
        message("getting cached matrix")
        return(inverse_value_in_the_matrix)
    }
    matrix_data <- x$get()
    inverse_value_in_the_matrix <- solve(matrix_data, ...)
    x$setinverse(inverse_value_in_the_matrix)
    inverse_value_in_the_matrix
}

mat1.data <- c(1,2,3,4)
mat1 <- matrix(mat1.data,nrow=2,ncol=2,byrow=TRUE)
mat1
cached_matrix <- makeCacheMatrix(mat1)
cacheSolve(cached_matrix)
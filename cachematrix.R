## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # assign the insvertedMatrix a value of NULL
    invertedMatrix <- NULL
    
    # define a function that sets the values of x and invertedMatrix in a different scope
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    
    # function that returns the value of x
    get <- function() x
    
    # function to set te inverted value of the matrix
    setInverseMatrix <- function(inverse) invertedMatrix <<- inverse
    
    # function to get the inverted matrix.
    getInverseMatrix <- function() invertedMatrix
    
    list (set=set, get=get,
          setInverseMatrix=setInverseMatrix,
          getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # assign a variable the inverted value of matrix x.
        invertedMatrix <- x$getInverseMatrix()
        
        #if the value is not null, that means it was cached.
        if (!is.null(invertedMatrix)) {
            # print a message that the cached value is being returned.
            message ("getting cached data")
            return(invertedMatrix)
        }
        # at this point, the value was not found in the cache. So, compute the inverse and return the value.
        data <- x$get()
        invertedMatrix <- solve(data)
        x$setInverseMatrix(invertedMatrix)
        invertedMatrix
}

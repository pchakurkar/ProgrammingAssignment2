makeCacheMatrix <- function(x = matrix()) {
    invertedMatrix <- NULL
    set <- function(y) {
        x <<- y
        invertedMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverse) invertedMatrix <<- inverse
    getInverseMatrix <- function() invertedMatrix
    
    list (set=set, get=get,
          setInverseMatrix=setInverseMatrix,
          getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertedMatrix <- x$getInverseMatrix()
    if (!is.null(invertedMatrix)) {
        message ("getting cached data")
        return(invertedMatrix)
        }
        data <- x$get()
    invertedMatrix <- solve(data)
        x$setInverseMatrix(invertedMatrix)
    invertedMatrix
}
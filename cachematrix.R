## makeCacheMatrix stores the list of functions 
## cacheSolve produces the inverse of function if cached in the makeCacheMatrix funtion

## cache of setMatrix, getMatrix, setInverse, getInverse funtions

makeCacheMatrix <- function(x=matrix()) { ## passing the matrix as an argument of a function
    m <- NULL ## Initializing value of m to NULL
    setMatrix <- function(y=matrix()){ ## set funtion to set the Matrix. This funtion is used to change the matrix later if required 
        x <<- y ## Assigning the value of y of setMatrix function to x which is the value of makeCacheMatrix funtion 
        m <<- NULL ##restroing the value of m to NULL
    }
    getMatrix = function() x ## getting what is set
    setInverse <- function(solve=matrix()) m <<- solve ## similar to setMatrix. Doesn't get the inverse but stores the value in m
    getInverse <- function() m ## similar to getMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,   ##stroing the functions in makeCacheMatrix in list
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of matrix using solve() funtion

cacheSolve <- function(x, ...) {
    m <- x$getInverse()  ## stores the value of getInverse from makeCacheMatrix function to m 
    if(!is.null(m)) { ## checking if value in m is not null
        message("getting cached data")
        return(m) ## printing the inverse of matrix
    }
    data <- x$getMatrix() ## if value of m is null, then it calculates the inverse of matrix. Gets the matrix
    m <- solve(data, ...) ## solve function is used to calculate the inverse of matrix
    x$setMatrix(m) ## Setting the inverse using setMatrix function.
    return(m) ## printing the inverse of matrix
}
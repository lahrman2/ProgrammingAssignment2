## Matrix inversion takes significant time and computer resources and it's advantageous to 
## cache the inverse of a matrix instead of recalculating it.  The following 2 functions will 
## cache the inverse of a matrix

## makeCacheMatrix does the following:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse of a matrix
## 4. ger the value of the invers of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## The below function returns the inverse of a matrix.  First it checks if the inverse
## was already calculated.  If so, it pulls the result and does not recalculate.  If not, 
## it calculates the inverse.  This function assumes the matrix can always be inverted.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null.(inv)) {
                message("gettinig cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}

##This function gives inverse of a matrix 
##Creates the cache of it rather than compute it repeatedly when ever required.
## If the inverse has already been calculated & the matrix has not changed then 
##the cache will retrieve the inverse from the cache 


## This function created a sepcial matrix, which contains a function to set 
##& get the values of matrix and mean of the same.

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function (t) {
                x <<- tmatrix
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function computes the inverse of special matrix created from 
##the previous function.
## The inverse is already calcualted so it get the inverse from the cache & skip 
##the computation of inverse of matrix & directly gets the inverse of matrix.

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}






## Overall goal is to create two functions. One, will cache the inverse of a matrix
## 2nd function will calculate the inverse of a matrix if the inverse is not already cached for that matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set <- function(y)
{
x<<-y
m<<-NULL
}
get<- function() x
setinverse <- function(z) m<<-z
getinverse<-function() m
list(set=set,get=get,setinverse =setinverse ,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cache solve should retrieve the inverse from the cache.
	
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


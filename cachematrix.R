## A couple of functions that cache the inverse of a matrix
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## Matrix initialization
  i  <- NULL
  ## Now setting the matrix
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
## Getting the matrix and returning it        
         get  <- function() { 
         x
         }
## Method to set the inverse of the matrix       
        setinverse  <- function(inverse) {
        i  <<- inverse
        }
## Now getting the inverse of a matrix and returning it        
        getinverse  <- function()  {
        i
        }
## Returning a list of the methods        
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}







## Compute the inverse of the special matrix returned by "makeCacheMatrix"  above
## If the inverse has already been calculated (and the matrix has not changed)
## then the "cachesolve" should give the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i  <- x$getinverse()
## Check if the inverse matrix is already there         
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
## Get the matrix from our object        
        data  <- x$get()
 ## Calculate the inverse using matrix multiplication        
        i  <- solve(data) %*% data
 ## Set the inverse to the object        
        x$setinverse(i)
## Return the martix        
        i
}

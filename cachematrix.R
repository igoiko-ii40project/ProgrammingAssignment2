## Put comments here that give an overall description of what your
## functions do
## 2020/12/26 Assignment 2: makeCacheMatrix
## SAMPLE for matrix inverses:
## x<-matrix(rnorm(16),4,4)
## y<-solve(x)                  #inverse matrix
## x%*%y                        #Identity matrix

## Write a short comment describing this function:
## Create the data structure for the matrix + inverse, and the functions associated to maintain the data
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_in) inverse <<- inverse_in
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function
## Return the inverse for a CacheMatrix data structure, either by retrieving from cache or Solving the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data for inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        message("solving data for inverse...")
        inverse
}

## CacheMatrix1=makeCacheMatrix(x)
## CacheMatrix1$get()
## cacheSolve(CacheMatrix1)
## CacheMatrix1$get()%*%cacheSolve(CacheMatrix1)
## git commit -a -m "updated"
## git push -u origin master  
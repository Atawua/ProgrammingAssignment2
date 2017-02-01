## The following fuctions calculate the inverse of a matrix and store its value to cache in order to return it if the user wishes,avoiding that way another time-consuming computation of the same inversed matrix.

## This function creates a special "matrix" object, which is really a list 

## containing a function to

## 1. set the value of the matrix

## 2. get the value of the matrix

## 3. set the value of the inverse

## 4. get the value of the inverse

## Create a function which takes a matrix named A as an argument
makeCacheMatrix <- function( A = matrix()) {
     
     ## set initial value of inv
     inv <- NULL
     
     set <- function(B) {
          
          ## assign the value of B to A (stored in the parent environment)
          A <<- B
          
          ## assign NULL in inv (stored in the parent environment)
          inv <<- NULL
     }
     ## return the matrix A
     get <- function() A
     
     ## sets the value of the inversed matrix to the inv variable (stored in the parent environment)
     setinverse <- function(inverse) inv <<- inverse
     
     ## return the cached value of inversed matrix A
     getinverse <- function() inv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix given from the makeCacheMatrix.If the inversed matrix is already calculated,it is being pulled from the cache.If not,the inversed matrix is calculated and then stored to cache from future usage.

## Create a function getting matrix A as argument along with other prossible arguments
cacheSolve <- function(A, ...) {
     
     ## assign the value of the cached inverse matrix from cacheMatrix to inv variable
     inv <- A$getinverse()
     
     ## if the value is different than null(meaning it was computated already),return the value of inversed matrix A
     if(!is.null(inv)) {
          message("Getting Cached Data")
          return(inv)
     }
     
     ## assign the value of A to a matrix named mymatrix 
     mymatrix <- A$get()
     
     ## store the value of inversed matrix A to inv variable
     inv <- solve(mymatrix)
     
     ## cache the inv variable for further usage and printing inv
     A$setinverse(inv)
     inv
}
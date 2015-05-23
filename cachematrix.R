## makeCacheMatrix function creates a special object that caches the inverse of a matrix. 
## So when the matrix inverse calculation is needed again, it can be looked up 
## in the cache rather than computed again saving time and computing resources.
## The function will then return a list of four functions:
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setinverse: set the value of the inverse
## 4. getinverse: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
                        x <<- y           
                        i <<- NULL
                }
                get <- function(){
                        x
                }
                setinverse <- function(inverse){
                        i <<- inverse
                }
                getinverse <- function() {
                        i
                }   
                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # return a list of functions
        }
        
## CacheSolve function calculates the inverse of the matrix passed to the function as argument x. 
## If the inverse of the matrix has been already calculated it will get it from the cache 
## and skip the computation. 
## If the inverse of the matrix is not in the cache, then it will 
## calculate the inverse and use the setinverse function to set the result in the cache.

cacheSolve <- function(x, ...) {

                i <- x$getinverse()
                if(!is.null(i)) {                       # if the inverse of the matrix was cached
                        message("getting cached data")
                        return(i)                       # return the inverse and exit the function
                }
                data <- x$get()                         # if the inverse of the matrix was not cached 
                i <- solve(data)                        # compute the inverse
                x$setinverse(i)                         # call function to cache the inverse
                i                                       # Return the inverse of matrix 'x'
        
}
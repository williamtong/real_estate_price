## Will Tong
## The objective of this is to check if the inverse of a matrix exists in the cache first 
## before calculating it.  
## If it does, then just output it from the cache.

makeCacheMatrix <- function(X = matrix()) {

## This function creates and returns a list of four sub-functions which are
        ## 1.   set:        Takes on the matrix X whose inverse it to be evaluated.
        ##                  Also sets the I (the inverse) of X to be NULL (for the time being till the inverse is calculated with the cacheSolve function)
        ## 2.   get:        Outputs the matrix that was set by the 'set' function.
        ## 3.   setinverse: Sets I when the value is passed by the input argument.
        ##                  Note this function does not actually calculate the inverse.
        ## 4.   getinverse: Outputs I
        I <- NULL
        set <- function(Y) {
                X <<- Y
                I <<- NULL
        }
        
        get <- function() X
        setinverse <- function(inv) {
                I <<- inv
        }
        
        getinverse <- function() I
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(X, ...) {
## This function accepts X which is an instance of makeCacheMatix and returns a matrix I that is the inverse of X$get().
        ## However, it first checks if I already exists in X by checking X$getinverse.
        ## If it does, then just output X$getinverse.
        ## If not, it calculates I using the inv function from the MASS library..
        ## ...and stores it in the cache by X$setinverse(I)

        I <- X$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
                
        }
        
        data <- X$get()
        require(MASS)
        I <- ginv(data)
        X$setinverse(I)
        I
}

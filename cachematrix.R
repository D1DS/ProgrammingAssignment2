###  The makeCachematrix function and cacheSolve function use the solve function provided in base R to cache the inversion of a matrix
##  and retrieve the inverse later.

## The makeCacheMatrix function calculates the inverse of matrix and caches it for future use

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #Create an empty matrix
        set <- function(y) { #Assign x to be the value of free variable y only within this environment
                x <<- y    #Make x available outside of this function environment (so that we can call it later in get)
                m <<- NULL #Make the empty matrix m available outside of this function environment (so we can use it in getsolve)
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve  #Set the inversion, place it in empty m matrix. Make m available outside of this function environment.
        getsolve <- function() m                 #Get the inversion
        
        list(set=set, get=get, setsolve = setsolve, getsolve = getsolve) #list the list of functions that are contained within the assigned object
}


## The cacheSolve function either gets the cached inverse matrix (if it exists), or calculates and returns the inverse for the matrix (if it was not previously cached)

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
        # Grab the getsolve function from makeCacheMatrix, assign it to m
        m<-x$getsolve()  
        #if the inverse matrix already exists, return "getting cached data", and then the cached matrix       
        if(!is.null(m)){
                message ("getting cached data") 
                return(m)       
        }
        # if m is not there, calculate the inverse of the matrix and return it
        data<-x$get() 
        m<-solve(data, ...)
        x$setsolve(m)
        m
}
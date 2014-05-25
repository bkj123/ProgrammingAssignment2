##MATRIX INVERSION 
##Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute 
##it repeatedly.  These 2 funtions cover this.

##here's an example of how to run it.
##1. highlight and run both functions
##2. a<-makeCacheMatrix()  ~
##3. a$set(matrix(rnorm(100), 10, 10)) ~ ##has to be a square matrix
##4. cacheSolve(a) ~

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<- solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##above. If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setsolve(m)
        m
}
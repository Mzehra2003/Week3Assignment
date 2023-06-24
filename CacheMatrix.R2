library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL       ##initialising inverse as NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x       ##function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inv<-ginv(x)
                    inv%*%x    ##function to obtain inverse of the matrix
                    }
  list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## This is used to retrieve the cache data

CacheSolve <- function(x, ...) ##to get cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){          ##checking whether "inverse" is NULL or not
                    message("getting cached data")
                    return(inv)    ##returns the inverse value
 }
 data<-x$get()
 inv<-solve(data,...)     ##calculates the inverse value
 x$setinv(inv)
 inv      ## returns a matrix that's the inverse of "x"
}

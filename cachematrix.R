## Abstracts a matrix representation into a cachable 
## enitity. In order to execute cacheSolve input must be a 
## cacheMatrix.
## eg. a <- matrix()
## ca <-makeCacheMatrix(a)
## solveCache(ca)


## Creates a cacheMatrix wrapper for a given input matrix
## no checks are made if this is a singular matrix, could prove useful
## to avoid errors donw the line

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      #initializing empty object
      x<<-y
      inv<<-NULL
    }
    get<- function() x
    setinv <- function(inverse) inv<<-inverse
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## Will accept input of type cacheMatrix as specified above
## solve function will be called if this is not cached already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
       
        if(!is.null(inv)){
            message("getting cached inverse")
            return(inv)
        }
        
        #fetch the actual matrix
        initobject<-x$get()
  
        #there is a possiblity that this could be a singular matrix
        #do we care as per the spec through? not really
        inv <-solve(initobject)
        
        #in case we get error the following line won't be executed
        #so we are safe not caching errors
        x$setinv(inv)
        inv
        
}

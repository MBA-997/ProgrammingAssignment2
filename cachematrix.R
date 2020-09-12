# The makecachematrix function creates object, functions, and 
# variables which are then returned to preserve values
# However the cachesolve function inverses a matrix and caches it
# to increase performance 

# This function creates an object x, and returns a list that contains functions
#and the values that resides in them

makeCacheMatrix <- function(x=matrix()){
    inv <- NULL
    set <- function(y)x<<-y
    get <- function()x
    set_inv <- function(i)inv<-i
    get_inv <- function()inv
    list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


#This function creates an inverse of matrix, and caches it to increase efficiency

cacheSolve <- function(x,...){
    inv<-x$get_inv()
    if(!is.null(inv)){
        print("Getting cached inverse")
        return(inv)
    }
    inv <- x$get()
    inv <- solve(inv,...)
    x$set_inv(inv)
    inv
}
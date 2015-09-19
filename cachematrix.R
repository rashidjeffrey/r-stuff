####################################################################################################
#
# WHAT  This file defines a pair of functions that cache the inverse of a matrix.
#       Matrix inversion is usually a costly computation and there may be some
#       benefit to caching the inverse of a matrix rather than computing it repeatedly.
#
# HOW   To test these functions enter the following blurb in the console and observe the results:
#       source('~/R/coursera/ProgrammingAssignment2/cachematrix.R')
#       testCache()
#
# REFERENCES
# Assignment  https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions
# GitHub repo https://github.com/rashidjeffrey/ProgrammingAssignment2
#
# WHEN      WHO     WHAT
# 20150820  Rashid  First draft. Defines the template as obtained from the assignment (see linked ref)
#                   Added annotations and made rudimentary changes to code, such as defining a matrix.
# 20150822  Rashid  Finish coding. Write a test function. 
#
####################################################################################################


####################################################################################################
# WHAT    This function creates a special "matrix" object that can cache its inverse.
#
# PARAM   Matrix x defaults to an empty matrix if argument value not specified
#
# RETURN  A list of functions which expose the public operations on the matrix.
#         The functions define getter (accessor) and setter (mutator) operations.
# 
#         get the value of the matrix
#         set the value of the matrix
#         get the value of the inversed matrix
#         set the value of the inversed matrix
#
makeCacheMatrix <- function(x = matrix()) {
        # Define an empty object will be used to store an inversed matrix
        inversedMatrix <- NULL
  
        # Accessor to return the current state of the matrix 
        getMatrix <- function() x
        
        # Mutator to
        # 1. Set the matrix object to the value of the parameter argument value
        # 2. Reset the inverse matrix to an empty object (because y is a new matrix)
        setMatrix <- function(pMatrix) {
                # The <<- operator assign a value to an object in a different environment
                x <<- pMatrix
                inversedMatrix <<- NULL
        }

        # Accessor to return the current state of the inversed matrix 
        getInversedMatrix <- function() inversedMatrix

        # Mutator to set the inversed matrix object to the value of the parameter argument value
        setInversedMatrix <- function(pInversedMatrix) inversedMatrix <<- pInversedMatrix
        
        # Return the accessors and mutators in a list
        list(getMatrix = getMatrix, 
             setMatrix = setMatrix, 
             getInversedMatrix = getInversedMatrix, 
             setInversedMatrix = setInversedMatrix)
        
}
####################################################################################################


####################################################################################################
# WHAT  This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#       If the inverse has already been calculated (and the matrix has not changed), 
#       then `cacheSolve` should retrieve the inverse from the cache.
#
# PARAM Matrix x returned by function makeCacheMatrix
#
# RETURN  Return a matrix that is the inverse of x
#
cacheSolve <- function(x, ...) {
  
        # Return a matrix that is the inverse of 'x'
        # Computing the inverse of a square matrix can be done with the `solve` function in R. 
        # For example, if `X` is a square invertible matrix, then `solve(X)` returns its inverse.
        
        # Get the inversed matrix
        inversedMatrix <- x$getInversedMatrix()
        
        # Is the inverse of the matrix cached?
        # i.e. is it a defined (non null) object?
        if (!is.null(inversedMatrix)) {
                message("Function cacheSolve. Returning cached inversed data...")
        }
        else {
                message("Function cacheSolve. Creating and caching inversed data...")
          
                # Get the matrix data
                data <- x$getMatrix()
                
                # Inverse the matrix 
                inversedMatrix <- solve(data, ...)
                
                # Cache the inversed matrix
                x$setInversedMatrix(inversedMatrix)       
        }

        return (inversedMatrix)
}
####################################################################################################

####################################################################################################
# WHAT  Test the speed of functions defined above
#       Cached data vs Non Cached data

testCache <- function() {
  #source('~/R/coursea/ProgrammingAssignment2/cachematrix.R')
  
  # Create a matrix of random numbers and cache it
  testMatrix <- matrix(rnorm(10000), nrow = 100, ncol = 100)
  cachedMatrix <- makeCacheMatrix(testMatrix)
  
  # Get the inverse of the matrix
  # Print how long it takes to do it the first time
  # when it is NOT yet cached
  timeBegin <- Sys.time()
  cacheSolve(cachedMatrix)
  timeEnd <- Sys.time()
  timeDuration <- timeEnd - timeBegin
  print(timeDuration)
  
  # Get the inverse of the matrix again
  # This time it should be quicker because the cached inverse matrix
  # should be returned
  timeBegin <- Sys.time()
  cacheSolve(cachedMatrix)
  timeEnd <- Sys.time()
  timeDuration <- timeEnd - timeBegin
  print(timeDuration)
  
}
###############################################################################################



"debye_1" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("debye_1",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}

"debye_2" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("debye_2",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

"debye_3" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("debye_3",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}

"debye_4" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("debye_4",
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(x)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(x)
  attributes(status) <- attributes(x)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}  

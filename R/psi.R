"psi_int" <- function(n, give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("psi_int",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(n)
  attributes(status) <- attributes(n)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}


"psi" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("psi",
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



"psi_1piy" <- function(y, give=FALSE, strict=TRUE){
  y.vec <- as.vector(y)
  jj <- .C("psi_1piy",
           as.double(y.vec),
           as.integer(length(y.vec)),
           val=as.double(y.vec),
           err=as.double(y.vec),
           status=seq(along=y.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(y)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(y)
  attributes(status) <- attributes(y)

  if(strict){
    val <- strictify(val,status)
  }

  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}


"psi_1_int" <- function(n, give=FALSE, strict=TRUE){
  n.vec <- as.vector(n)
  jj <- .C("psi_1_int",
           as.integer(n.vec),
           as.integer(length(n.vec)),
           val=as.double(n.vec),
           err=as.double(n.vec),
           status=seq(along=n.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attributes(n)
  err <- jj$err
  status <- jj$status
  attributes(err) <- attributes(n)
  attributes(status) <- attributes(n)

  if(strict){
    val <- strictify(val,status)
  }
  
  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}

"psi_1" <- function(x, give=FALSE, strict=TRUE){
  x.vec <- as.vector(x)
  jj <- .C("psi_1",
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

"psi_n" <- function(m, x, give=FALSE, strict=TRUE){
  jj <- process.2.args(m,x)
  m.vec <- jj$arg1
  x.vec <- jj$arg2
  attr <- jj$attr
  jj <- .C("psi_n",
           as.integer(m.vec),
           as.double(x.vec),
           as.integer(length(x.vec)),
           val=as.double(x.vec),
           err=as.double(x.vec),
           status=seq(along=x.vec),
           PACKAGE="gsl"
           )
  val <- jj$val
  attributes(val) <- attr
  err <- jj$err
  status <- jj$status
  attributes(err) <- attr
  attributes(status) <- attr

  if(strict){
    val <- strictify(val,status)
  }

  if(give){
      return(list(val=val,err=err,status=status))
  } else {
    return(val)
  }
}

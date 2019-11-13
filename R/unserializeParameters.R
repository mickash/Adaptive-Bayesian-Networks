unserializeParameters <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  UseMethod('unserializeParameters', object)
}
unserializeParameters.dirichlet <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1
  object$pvals<-Parameters2
  return (object)
}
unserializeParameters.flag <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1+1
  return (object)
}
unserializeParameters.logand <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1+1
  return (object)
}
unserializeParameters.noisyor <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1
  object$pindices=Parameters2+1
  object$params2=Parameters3
  object$updateDisambiguation=ifelse(miscString,'T','F')
  return (object)
}
unserializeParameters.logprod <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  return (object)
}
unserializeParameters.flagprod <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1+1
  return (object)
}
unserializeParameters.logor <- function (
  object,
  Parameters1,
  Parameters2,
  Parameters3,
  miscString)
{
  object$params=Parameters1+1
  return (object)
}

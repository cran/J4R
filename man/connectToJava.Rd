% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/j4rConnection.R
\name{connectToJava}
\alias{connectToJava}
\title{Connect to Java environment}
\usage{
connectToJava(
  port = c(0, 0),
  extensionPath = NULL,
  memorySize = NULL,
  debug = FALSE
)
}
\arguments{
\item{port}{a vector of the listening ports for the Java server}

\item{extensionPath}{the path to jar files that can be loaded by the system classloader}

\item{memorySize}{the memory size of the Java Virtual Machine in Mb (if not specified, the JVM runs with the default memory size)}

\item{debug}{for debugging only (should be left as is)}
}
\value{
a logical TRUE if the function managed to get connected to the server or if it was already connected or
FALSE if the connection has failed
}
\description{
This function connects the R environment to a gateway server that runs in Java.
}
\details{
The first argument of the function provides the listening port for the Java server. A maximum of four ports is
allowed. When set to 0, these ports are randomly selected. By default, the server listens to two random
ports.

The extensionPath can either be set in this function or dynamically changed (see the addToClassPath function).
}
\seealso{
\code{\link{addToClassPath}}
}

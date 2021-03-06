% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/j4rConfig.R
\name{getJavaVersion}
\alias{getJavaVersion}
\title{Get the current Java version}
\usage{
getJavaVersion()
}
\value{
a list with the first slot (version) being the version and the second slot (architecture)
referring to the 32-Bit or 64-Bit architecture
}
\description{
Returns the current Java version either through the command line if not connected to
the Java server or through the Java server if connected.
}
\seealso{
getJavaArchitecture
}

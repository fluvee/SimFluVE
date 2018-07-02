\name{PSCestimation}
\alias{PSCestimation}
\title{Estimate VE from Passive Surveillance Cohort Study (PSC)}
\usage{
PSCestimation(N, SENS, SPEC, outcomes)
}
\arguments{
\item{N}{number of sample size}

\item{SENS}{sensitivity}

\item{SPEC}{specificity}

\item{outcomes}{generated outcome file}
}
\details{
1.	User Setup: Set up cohort size, sensitivity and specificity.
2.	Estimate VE
}
\value{
Mean and std of estimated VE
}

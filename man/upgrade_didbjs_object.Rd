\name{upgrade_didbjs_object}
\alias{upgrade_didbjs_object}
\title{Upgrade a didbjs result object}
\usage{
upgrade_didbjs_object(x)
}
\arguments{
\item{x}{A list-like \code{didbjs} result object.}
}
\description{
Normalize older compatible \code{didbjs} result objects to the current
\code{didbjs.result.v1} object contract by restoring optional fields, class,
and version metadata. Objects missing required estimator fields fail closed with
\code{didbjs_contract_error}.
}
\value{
A \code{didbjs} result object with current version metadata.
}

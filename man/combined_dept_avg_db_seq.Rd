% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by_groups.R
\name{combined_dept_avg_db_seq}
\alias{combined_dept_avg_db_seq}
\title{Compute combined dataframe of a sequence of two courses}
\usage{
combined_dept_avg_db_seq(
  seq_db = list(db1 = df_700_703, db2 = df_p7_to_8),
  db_compared = df_902_903,
  gp_names = c("p7to8", "902/903"),
  running_window = running_DFW_terms
)
}
\arguments{
\item{seq_db}{list of the sequence of two courses}

\item{db_compared}{compared database}

\item{gp_names}{}

\item{running_window}{running window size}
}
\value{

}
\description{
Compute combined dataframe of a sequence of two courses
}

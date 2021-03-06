% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by_faculty.R
\name{plot_fac_DFW_by_group}
\alias{plot_fac_DFW_by_group}
\title{plot DFW of a group of faculty or individual}
\usage{
plot_fac_DFW_by_group(
  DFW_data = by_faculty_DFW,
  avg_data = MATH_dept_fac_avg,
  grp,
  grp_nm = grp,
  TYPE = "Dept avg",
  earliest_sem = earliest_sem_in_plot
)
}
\arguments{
\item{DFW_data}{input dataframe}

\item{avg_data}{input aggregated (department) dataframe}

\item{grp}{selected faculty members}

\item{grp_nm}{}

\item{TYPE}{"Dept avg" or other}

\item{earliest_sem}{eariest semester to display}
}
\value{

}
\description{
plot DFW of a group of faculty or individual
}

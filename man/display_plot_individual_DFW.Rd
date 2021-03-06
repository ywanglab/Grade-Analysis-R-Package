% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/by_faculty.R
\name{display_plot_individual_DFW}
\alias{display_plot_individual_DFW}
\title{Display the table and Plot DFW of individual faculty}
\usage{
display_plot_individual_DFW(
  individual,
  DFW_data = by_faculty_DFW,
  avg_data = MATH_dept_fac_avg,
  input_table = by_fac_table,
  TYPE = "Dept avg",
  earliest_sem = earliest_sem_in_plot
)
}
\arguments{
\item{individual}{individual name}

\item{DFW_data}{input dataframe}

\item{avg_data}{input aggregated (department) dataframe}

\item{input_table}{input displayed table in wide format}

\item{TYPE}{"Dept avg" or other}

\item{earliest_sem}{earliest semester to display}
}
\value{

}
\description{
Display the table and Plot DFW of individual faculty
}

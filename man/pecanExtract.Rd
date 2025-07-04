% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pecanExtract.R
\name{pecanExtract}
\alias{pecanExtract}
\title{Title}
\usage{
pecanExtract(
  data,
  id_row = 1,
  nodes,
  edges,
  edges_direction = "from_to",
  edges_sep,
  nodes_labels = NULL,
  label_pattern = NULL,
  nodes_attributes = NULL,
  attribute_pattern = NULL,
  drop_nodes_NA0 = "na_zero",
  drop_edges_NA0 = "na_zero",
  drop_attributes_NA0 = "na_zero"
)
}
\arguments{
\item{data}{This parameter should be a dataframe where each row represents a person or timepoint (can also just be one row),
and the columns contain specific values for the nodes and edges corresponding to that person or timepoint.}

\item{nodes}{Specifies the positions of nodes columns in the dataframe. E.g. 1:10}

\item{edges}{Specifies the positions of edges columns in the dataframe. E.g. 11:100.  Each edge should be represented by a
combination of node names separated by a specified separator. For example, if an edge connects nodes "pain" and "insomnia",
it could be represented as "pain_insomnia" in the dataframe.}
}
\description{
Title
}

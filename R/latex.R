# From bookdown/R/latex.R
resolve_refs_latex = function(x) {
  # equation references \eqref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\((eq:[-/:[:alnum:]]+)\\)', '\\\\eqref{\\1}', x,
    perl = TRUE
  )
  # normal references \ref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\(([-/:[:alnum:]]+)\\)', '\\\\ref{\\1}', x,
    perl = TRUE
  )
  x = gsub(sprintf('\\(\\\\#((%s):[-/[:alnum:]]+)\\)', "fig|tab|eq|thm|lem|cor|prp|cnj|def|exm|exr|hyp|ex"), '\\\\label{\\1}', x)
  x
}

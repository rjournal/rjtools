local replace_cranpkg = {
  match = '\\CRANpkg%{(%w+)%}',
  html = 'https://cran.r-project.org/package=%s',
  pdf = '\\CRANpkg{%s}'
}
local replace_biopkg = {
  match = '\\BIOpkg%{(%w+)%}',
  html = 'https://www.bioconductor.org/packages/%s/',
  pdf = '\\BIOpkg{%s}'
}
local replace_pkg = {
  match = '\\pkg%{(%w+)%}',
  html = '%s',
  pdf = '\\pkg{%s}'
}

function RawInline(elem)
  local out = {}
  
  local cranpkg = string.match(elem.text, replace_cranpkg.match)
  local biopkg = string.match(elem.text, replace_biopkg.match)
  local pkg = string.match(elem.text, replace_pkg.match)
  if pkg ~= nil then
    if FORMAT:match 'html.*' then
      table.insert(out, pandoc.Link(pkg, '#'))
    else
      table.insert(out, pandoc.RawInline(string.format(replace_pkg.pdf, pkg)))
    end
  elseif cranpkg ~= nil then
    if FORMAT:match 'html.*' then
      table.insert(out, pandoc.Link(cranpkg, string.format(replace_cranpkg.html, cranpkg)))
    else
      table.insert(out, pandoc.RawInline(string.format(replace_cranpkg.pdf, cranpkg)))
     end
  elseif biopkg ~= nil then
    if FORMAT:match 'html.*' then
      table.insert(out, pandoc.Link(biopkg, string.format(replace_biopkg.html, biopkg)))
    else
      table.insert(out, pandoc.RawInline(string.format(replace_biopkg.pdf, biopkg)))
    end
  end
  
  if #(out) == 0 then
    return elem
  end
  return out
end

 
function Header (h)
  if h.level > 1 then 
    h.classes:insert 'unnumbered'
  end
  return h
end

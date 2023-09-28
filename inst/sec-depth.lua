
function Header (h)
  if h.level > 2 then
    h.classes:insert 'unnumbered'
  end
  return h
end

function Pandoc (doc)
  -- Create and number sections. Setting the first parameter to
  -- `true` ensures that headings are numbered.
  doc.blocks = pandoc.utils.make_sections(true, nil, doc.blocks)

  -- Shift the heading levels by 1
  doc.blocks = doc.blocks:walk {
    Header = function (h)
      if h.level > 2 then
        h.classes:insert 'unnumbered'
      end
      h.level = h.level + 1
      return h
    end
  }

  -- Return the modified document
  return doc
end

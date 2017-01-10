class STACK_Q

module CDATAUtil
  def cdata(s)
    "<![CDATA[" + esq_cdata(s) + "]]>"
  end

  def esq_cdata(s)
    (s || "").gsub("]]>", "]]]]><![CDATA[>")
  end
end

end

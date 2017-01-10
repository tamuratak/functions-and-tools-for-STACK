require "erb"

class STACK_Q

module CDATAUtil
  def cdata(s)
    "<![CDATA[" + esq_cdata(s) + "]]>"
  end

  def esq_cdata(s)
    (s || "").gsub("]]>", "]]]]><![CDATA[>")
  end
end

module StackqUtil
include ERB::Util
include CDATAUtil
  module_function

  def does_satisfy_ex(ext)
    if /\A\(.*?\)\s*((and|or)\s*\(.*?\))*\z/ =~ ext
      ext.gsub(/\((.*?)\)\s*(and|or|\z)/){|s|
        e1 = $1
        e2 = $2
        if /\Anot (.*)/ =~ e1
          "not does_hold(" + $1 + ") " + e2
        else
          "does_hold(" + e1 + ") " + e2
        end
      }
    else
      raise "format invalid for does_satisfy"
    end
  end

  def is_matrix_type(a)
    if /\Amatrix/ =~ a
      a = a.gsub(/\s+/, "")
      7.times{
        a = a.gsub(/\([^\(\)]*\)/, "")
      }
      "matrix" == a
    else
      false
    end
  end

  def one_input(name, type, dims: nil, input_size: 15)
    if type == "matrix"
      cols, rows = dims
      tmp = "[" + n_join(rows, "1", ",") + "]"
      tans = "matrix(" + n_join(cols, tmp, ",") + ")" # dummy
    else
      tans = "1" # dummy
    end
    ERB.new(<<HERE, nil, '-').result(binding)
    <input>
      <name><%= name %></name>
      <type><%= type %></type>
      <tans><%= tans %></tans>
      <boxsize><%= input_size %></boxsize>
      <strictsyntax>1</strictsyntax>
      <insertstars>0</insertstars>
      <syntaxhint></syntaxhint>
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]],min,max </forbidwords>
      <allowwords></allowwords>
      <forbidfloat>1</forbidfloat>
      <requirelowestterms>0</requirelowestterms>
      <checkanswertype>0</checkanswertype>
      <mustverify>1</mustverify>
      <showvalidation>1</showvalidation>
      <options></options>
    </input>
HERE
  end

  def multi_input(arry)
    ret = ""
    arry.each{|e|
      name, type, dims, input_size = e
      ret << one_input(name, type, dims: dims, input_size: input_size)
    }
    ret
  end

  def n_join(n, str, sp = ", ")
    (1..n).map{|i| str % i }.join(sp)
  end

  def basis_dim(s)
    if m = s.match(/\[([^\[\]]*?)\]/)
      $1.split(",").size
    end
  end

  def varname(name, idx = nil)
    if idx
      "#{name}_#{idx}"
    else
      name
    end
  end

  def desc_varnames_forms(desc_varnames, idx: nil, nline: nil)
    ERB.new(<<HERE, nil, '-').result(binding).chop
<p>
<%     desc_varnames.each do |desc0, name0| -%>
<%=h desc0  %> [[input:<%= varname(name0, idx) %>]] &nbsp;&nbsp;&nbsp;<% if nline %><br><% end %>
<%     end -%>
</p>
<div>
<%     desc_varnames.each do |desc0, name0| -%>
[[validation:<%= varname(name0, idx) %>]]
<%     end -%>
</div>
<br><br>
HERE
  end

end

end

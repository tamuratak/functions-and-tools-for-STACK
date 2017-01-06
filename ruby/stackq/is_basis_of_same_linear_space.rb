require "stackq/base"
require "stackq/basisutil"

class STACK_Q

class Is_basis_of_same_linear_space < StackqBase
  include BasisUtil

  def initialize(*args)
    super
    basis_type_check(@a1, @line_num)
    @dim = basis_dim(@a1)
  end

  def basis_ans_form(dim)
    n_join(dim, "[[input:ans%d]]", " ")
  end

  def basis_validation_form(dim)
    n_join(dim, "[[validation:ans%d]]", " ")
  end

  def ans_inputs(prefix="")
    ret = ""
    (1..@dim).each do |i|
      ret << one_input("ans"+prefix+i.to_s, "matrix", dims: [@dim, 1], input_size: @input_size)
    end
    ret
  end

  def feedbk
    b1 = n_join(@dim, "list_matrix_entries(ans%d)", ", ") # b1 == ans1 == student's answer
    large_Ns = n_join(@dim, "N", ", ")
    basis_chk =
      case @mthd
      when "is_basis_of_same_linear_space"
        "is_basis"
      when "is_orthonormal_basis_of_same_linear_space"
        "is_orthonormal_basis"
      else
        raise
      end
    ERB.new(<<HERE, nil, '-').result(binding).chomp
<![CDATA[
<%= basis_feedback_lib_mac() %>
b1 : delete([<%= large_Ns %>], [<%= b1 %>]);
result : if is_same_linear_space(k1, b1) and <%= basis_chk %>(b1) then true else false;
]]>
HERE
  end

  def ans_forms
    ret = ERB.new(<<HERE, nil, '-').result(binding).chomp
<p> <%= basis_ans_form(@dim) %></p>
<div><%= basis_validation_form(@dim) %></div>
HERE
  end

  def basis_type_check(s, line_num)
    unless /\A\[(\[[^\[\]]*?\],?\s*)*\]\Z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
    arry = s.scan(/\[[^\[\]]*?\]/)
    siz = arry[0].split(",").size
    arry.each{|e|
      unless siz == e.split(",").size
        @err_msg = "error at line: #{line_num}" + "\n" + "dimensions of basis are different"
        raise "invalid answer type"
      end
    }
  end

end

end

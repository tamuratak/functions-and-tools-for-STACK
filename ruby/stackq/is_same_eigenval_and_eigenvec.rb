# -*- coding: utf-8 -*-
require "stackq/base"
require "stackq/basisutil"

class STACK_Q

class Is_same_eigenval_and_eigenvec < StackqBase
include BasisUtil

  def initialize(*args)
    super
    @eigen_val_num, @dim = eigen_num_dim(@a1)
  end
  attr_reader :eigen_val_num

  def basis_ans(n, dim, input_size, prefix="")
    ret = ""
    (1..n).each do |i|
      ret << one_input("ans"+prefix+i.to_s, "matrix", dims: [dim, 1], input_size: input_size)
    end
    ret
  end

  def eigen_num_dim(s)
    vecs = []
    arry = s.scan(/\[(.*?), \[\s*((?:\[.*?\],?)+)\s*\]\s*\]/)
    arry.each{|e|
      vecs += e[1].scan(/\[.*?\]/).map{|s| s.split(",") }
    }
    vecs_sizes = vecs.map{|e| e.size }
    unless vecs_sizes.uniq.size == 1
      raise "the dims of eigen vectors are not the same"
    end
    dim = vecs_sizes[0]
    eigen_val_num = arry.size
    return *[eigen_val_num, dim]
  end

  def eigen_ans_arry(dim, idx)
    "[" + n_join(dim, "ans#{idx}_%d") + "]"
  end

  def feedbk
    ans_vals = n_join(@eigen_val_num, "ans_val%d") # ans_val == ans_eigenval
    large_Ns = n_join(@dim, "N", ", ")
    ERB.new(<<HERE, nil, '-').result(binding).chomp
<![CDATA[
<%= basis_feedback_lib_mac() %>
ith : 0;
result : is(<%= @eigen_val_num %> = length(unique([<%= ans_vals %>])));
ith : if result then ith + 1 else ith;
<%- (1..@eigen_val_num).each do |i| -%>
vec<%= i %> : delete([<%= large_Ns %>], maplist(list_matrix_entries, <%= eigen_ans_arry(@dim,i) %>));
kvec<%= i %> : assoc(ans_val<%= i %>, k1);
result : result and listp(kvec<%= i %>) and <%= basis_chk(mthd) %>(vec<%= i %>) and is_same_linear_space(kvec<%= i %>, vec<%= i %>);
ith : if result then ith + 1 else ith;
<%- end -%>
]]>
HERE
  end

  def eigen_val_inputs(i)
    one_input("ans_val#{i}", "algebraic")
  end

  def ans_inputs
    ret = ""
    (1..@eigen_val_num).each{|i|
      ret << eigen_val_inputs(i)
      ret << basis_ans(@dim, @dim, @input_size, "#{i}_")
    }
    ret
  end

  def ans_forms
    ret = ""
    (1..@eigen_val_num).each{|idx|
      ans = n_join(@dim, "[[input:ans#{idx}_%d]]", " ")
      valid = "[[validation:ans_val#{idx}]] "
      valid += n_join(@dim, "[[validation:ans#{idx}_%d]]", " ")
      ret << ERB.new(<<HERE, nil, '-').result(binding)
<p> 固有値 [[input:ans_val<%= idx %>]] それに対する固有ベクトルは次のベクトルの1次結合である <%= ans %></p>
<div><%= valid %></div><br><br>
HERE
    }
    ret
  end

end

end

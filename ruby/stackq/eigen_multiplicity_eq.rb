# -*- coding: utf-8 -*-

class STACK_Q

class Eigen_multiplicity_eq < StackqBase

  def initialize(*args)
    super
    @ans_num, @ans_dim = eigen_multiplicity_num_dim(@a1)
    @desc_varnames = [["固有値", "eigenval"], ["重複度", "chofuku"], ["固有空間の次元", "jigen"]]
  end

  def eigen_val_num
    @ans_num
  end

  def eigen_multiplicity_num_dim(s)
    vecs = []
    arry = s.scan(/\[.*?\]/)
    arry.each{|e|
      vecs << e.split(",")
    }
    vecs_sizes = vecs.map{|e| e.size }
    unless vecs_sizes.uniq.size == 1
      raise "the dims of eigen vectors are not the same"
    end
    return *[arry.size, vecs_sizes[0]]
  end

  def eigen_multiplicity_check_size(ans_dim, desc_varnames)
    unless ans_dim == desc_varnames.size
      raise "ans_dim and the size of desc_varnames are not the same"
    end
  end

  def ans_inputs
    ret = ""
    (1..@ans_num).each{|i|
      @desc_varnames.each{|desc0, name0|
        ret << one_input(varname(name0, i), "algebraic", input_size: 15)
      }
    }
    ret
  end

  def feedbk
    ERB.new(<<HERE, nil, '-').result(binding).chomp
<![CDATA[
#{does_hold_mac}
sans1 : stackqsimp(<%= varnames_matrix(@desc_varnames, @ans_num) %>);
ith : 0;
result : is(<%= @ans_num %> = length(unique(sans1)));
<% (1..@ans_num).each do |idx| -%>
ith : if result then ith + 1 else ith;
sans0 : <%= varnames_arry(@desc_varnames, idx) %>;
result : result and some(lambda([x], does_hold(sans0 = x)), k1);
<% end -%>
]]>
HERE
  end

  def ans_forms
    ERB.new(<<HERE, nil, '-').result(binding)
<% (1..@ans_num).each do |idx| %>
<%= desc_varnames_forms(@desc_varnames, idx: idx) %>
<% end -%>
HERE
  end

end

end

# -*- coding: utf-8 -*-
require 'erb'
require 'stack_q_tmpl'
require "stackq/base"
require "stackq/is_P_and_PAP"
require "stackq/eigen_multiplicity_eq"

class STACK_Q
include CDATAUtil

  def initialize(s, opt = {})
    @txt = s
    @err_msg = ""
    @category = opt["category"] || "stack_q"
    @opt = opt
  end
  attr_reader :err_msg

  def txt2xml
    ret = ""
    ret << ERB.new(HEAD).result(binding)
    line_num = 1
    @sort_prefix0 ||= sort_prefix()

    @txt.each_line{|l|
      next if /\A\s*\Z/ =~ l
      @err_msg = "error at line: #{line_num}"

      qname, qstr, a1, mthd, ext = l.split(/\s*\*\*\s*/).map{|s| s.sub(/\A\s*/, "").sub(/\s*\Z/, "") }
      mthd = mthd || "AlgEquiv"
      validate_maxima_exp(a1, line_num, l)

      # workaround for Moodle + MathJax bug
      qstr = '\(\,\)  ' + inline_tex(qstr)

      # teacher's answer == a1 == t_ans1, (prt stands for potential response tree)
      # student's answer == ans1
      unless ret0 = ( txt2xml_with_single_input(qname, qstr, a1, mthd, ext, line_num) or
                      txt2xml_with_multi_input(qname, qstr, a1, mthd, ext, line_num) )
        raise "invalid grading method"
      end

      ret << ret0
      line_num  += 1
    }
    
    ret << FOOT
  end

  def qname_0(qname, line_num)
    if @opt["sort-prefix"]
      ln = "%.2d" % line_num
      qname_0 = @sort_prefix0 + "-" + ln + "-" + qname
    else
      qname_0 = qname
    end
  end

  def txt2xml_with_single_input(qname, qstr, a1, mthd, ext, line_num)
    x = ERB.new(TMPL)
    qname_0 = qname_0(qname, line_num)
    input_size = @opt["form-size"] || 15

    case mthd
    when "AlgEquiv", "CasEqualNotAsin"
      klass = AlgEquiv
    when "is_same_linear_eq", "has_same_nullspace", "is_same_plane"
      klass = Is_same_linear_eq
    when "has_same_deriv"
      klass = Has_same_deriv
    when "does_satisfy"
      klass = Does_satisfy
    when "is_same_diag"
      klass = Is_same_diag
    else
      return nil
    end

    quiz = klass.new(a1, input_size: input_size, mthd: mthd, ext: ext)
    t_ans1 = quiz.t_ans1
    feedbk = quiz.feedbk
    stack_mthd = quiz.stack_mthd
    forbidwords = quiz.forbidwords
    input_type = quiz.input_type
    input_size = quiz.input_size

    x.result(binding)
  end

  # we have to set (inputs + feedback + answer forms
  # inputs == student answer type + form size + etc
  def txt2xml_with_multi_input(qname, qstr, a1, mthd, ext, line_num)
    qname_0 = qname_0(qname, line_num)

    input_size = @opt["form-size"] || 15

    case mthd
    when "eigen_multiplicity_eq"
      klass = Eigen_multiplicity_eq
      x = ERB.new(TMPL_multi, nil, '-')

    when "is_P_and_PAP"
      klass = Is_P_and_PAP
      x = ERB.new(TMPL_basis, nil, '-')

    when "is_basis_of_same_linear_space", "is_orthonormal_basis_of_same_linear_space"
      klass = Is_basis_of_same_linear_space
      x = ERB.new(TMPL_basis, nil, '-')


    when "is_same_eigenval_and_eigenvec", "is_same_eigenval_and_orthonormal_eigenvec"
      klass = Is_same_eigenval_and_eigenvec
      x = ERB.new(TMPL_eigen, nil, '-')

    else
      return nil
    end
    quiz = klass.new(a1, input_size: input_size)
    quiz.mthd = mthd
    ans_inputs = quiz.ans_inputs
    feedbk = quiz.feedbk
    ans_forms = quiz.ans_forms

    x.result(binding)
  end

  def inline_tex(s)
    s.gsub(/([^\\]|\A)\$((\\\$|[^\$])*)\$/) { $1 + '\\(' + $2 + '\\)' }
  end
  
  def validate_maxima_exp(s, line_num = 1, l = "")
    tmp = s
    until tmp == " XXX "
      prev = tmp
      tmp = tmp.gsub(/(?<=\A|[\(\[\{,]|and|or|not)\s*-?(\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*([\*\+\-\^\/\=]|[\>\<]=?))*\s*([a-zA-Z]\w*\!?|\d+\!?|%e|%pi|%i)\s*(?=\z|[\)\]\},]|and|or)/, " XXX ")
      5.times{
        tmp = tmp.gsub(/(?!(and|or|not)\s*\()([a-z]{3,})\s*\(( XXX ,)* XXX \)/, " XXX ")
      }
      tmp = tmp.gsub(/\( XXX \)/, " XXX ")
      tmp = tmp.gsub(/\[( XXX ,)* XXX \]/, " XXX ")
      tmp = tmp.gsub(/\{( XXX ,)* XXX \}/, " XXX ")
      tmp = tmp.gsub(/ XXX (and|or) XXX /, " XXX ")
      tmp = tmp.gsub(/not XXX/, " XXX ")
      if tmp == prev
        @err_msg = "validation error of maxima expression at line #{line_num}" + "\n"
        raise "\n\n" + @err_msg + l + "\n\n"
      end
    end

    return true
  end

  def sort_prefix
    today = Time.now
    if [1, 2, 3].include?( today.month )
      num = Time.new(today.year, 4, 1) - today
    else
      num =  Time.new(today.year+1, 4, 1) - today
    end
    num = num.round / (60*60)
    "%.4d" % num
  end
 
  def plane_type_check(s, line_num)
    unless /\A\[[^\[\]]*?\]\z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
  end

  def eq_type_check(s, line_num)
    unless /\A\{[^\{\}]*?\}\z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
  end

module BasisUtil

  def basis_feedback_lib_mac
<<HERE.chomp
#{does_hold_mac}
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(radcan(a)),x0 : listify(radcan(x)),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
is_basis(x) := block([xm],xm : apply(matrix, x),is( rank(xm) = length(x) ));
is_orthonormal_basis(x) := block([xm, tmp],xm : apply(matrix, radcan(x)),tmp : xm.(conjugate(transpose(xm))),does_hold( ident(length(x)) = tmp ) or does_hold( 1 = tmp ));
HERE
  end

  def basis_ans(n, dim, input_size, prefix="")
    ret = ""
    (1..n).each do |i|
      ret << one_input("ans"+prefix+i.to_s, "matrix", dims: [dim, 1], input_size: input_size)
    end
    ret
  end

  def basis_chk(mthd)
    case mthd
    when /orthonormal/
      "is_orthonormal_basis"
    else
      "is_basis"
    end
  end

end

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

class AlgEquiv < StackqBase
  def initialize(*args)
    super
  end

  def t_ans1
    cdata(@a1)
  end

  def feedbk
    <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
  end

  def stack_mthd
    if @mthd == "CasEqualNotAsin"
      "CasEqual"
    else
      @mthd
    end
  end

  def forbidwords
    if @mthd == "CasEqualNotAsin"
      ",asin,acos,atan"
    else
      ""
    end
  end
end

class Is_same_linear_eq < StackqBase
  def initialize(*args)
    super
    eq_type_check(@a1, @line_num) if @mthd == "is_same_linear_eq"
  end

  def eq_type_check(s, line_num)
    unless /\A\{[^\{\}]*?\}\z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
  end

  def t_ans1
    case @mthd
    when "is_same_plane"
      cdata("transpose(matrix(" + @a1 + "))")
    else
      cdata(@a1)
    end
  end

  def feedbk
    ret = ""
    ret <<
<<EOS
<![CDATA[
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(radcan(a)),x0 : listify(radcan(x)),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
basis_of_plane(v) := block([params],params : listofvars(v),map(lambda([v1], diff(v, v1)), params));
pos_of_plane(v) := block([v0 : v, params, i],params : listofvars(v),for i:1 thru length(params) do v0 : subst(0, params[i], v0),v0);
is_on_plane(p, v) := block([eq],eq : map("=", makelist(0, length(v)), v-p),is(not ([] = solve(eq, listofvars(v)))));
is_same_plane(v1, v2) := block([b1, b2, p1, p2, ret : true],b1 : basis_of_plane(v1),b2 : basis_of_plane(v2),ret : ret and is_same_linear_space(b1, b2),ret : ret and is_on_plane(pos_of_plane(v1), v2),ret : ret and is_on_plane(pos_of_plane(v2), v1));
eq_to_param(eq) := block([params, tmp],eq : listify(eq),params : sort(listofvars(eq)),tmp : solve(eq, params),subst(tmp[1], params));
is_same_linear_eq(eq1, eq2) := block([pa1, pa2],pa1 : eq_to_param(eq1),pa2 : eq_to_param(eq2),is_same_plane(pa1, pa2));

EOS
      # ]]> should be added in the following.
    case @mthd
    when "is_same_linear_eq"
      ret <<
<<EOS.chomp
a1 : #{esq_cdata(@a1)};
result : if is_same_linear_eq(a1, ans1) then 1 else false;
]]>
EOS
    when "is_same_plane"
      ret <<
<<EOS.chomp
a1 : #{esq_cdata(@a1)};
ans1 : list_matrix_entries(ans1);
result : if is_same_plane(a1, ans1) then 1 else false;
]]>
EOS
    when "has_same_nullspace"
      ret <<
<<EOS.chomp
a1 : #{esq_cdata(@a1)};
result : if is_same_linear_space(args(a1), args(ans1)) then 1 else false;
]]>
EOS
    else
      raise 
    end
    return ret
  end

  def input_type
    case @mthd
    when "is_same_plane"
      "matrix"
    else
      super
    end
  end

  def input_size
    case @mthd
    when "is_same_plane"
      15
    else
      100
    end
  end
end

class Has_same_deriv < StackqBase

  def feedbk
      <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
a1 : diff(a1,x);
ans1 : diff(ans1, x);
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
  end

  def stack_mthd
    "AlgEquiv"
  end
end

class Does_satisfy < StackqBase

  def feedbk
      <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
result : if #{esq_cdata(does_satisfy_ex(@ext))} then 1 else false;
]]>
EOS
  end

end

class Is_same_diag < StackqBase

  def feedbk
      <<EOS.chomp
<![CDATA[
is_diagonal(m) := block([col_size, row_size],col_size : length(m),row_size : length(m[1]),is(col_size = row_size) and is( m = m * diagmatrix(col_size, 1)));
get_diag_element(m) := block([len, i],len : length(m),maplist(lambda([i], m[i,i]), makelist(i, i, len)));
is_same_diag(a, x) := block([],is_diagonal(a) and is_diagonal(x) and does_hold( sort(get_diag_element(a)) = sort(get_diag_element(x)) ));
#{does_hold_mac}

a1 : #{esq_cdata(@a1)};
result : if is_same_diag(a1, ans1) then 1 else false;
]]>
EOS
  end

  def input_size
    15
  end

  def input_type
    "matrix"
  end

end

end

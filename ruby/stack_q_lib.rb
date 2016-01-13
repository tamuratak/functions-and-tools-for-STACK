# -*- coding: utf-8 -*-
require 'erb'
require 'stack_q_tmpl'

class STACK_Q
  include ERB::Util

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
      unless ret0 = ( txt2xml_single(qname, qstr, a1, mthd, ext, line_num) or
                      txt2xml_multi(qname, qstr, a1, mthd, ext, line_num) )
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

  def txt2xml_single(qname, qstr, a1, mthd, ext, line_num)
    x = ERB.new(TMPL)
    input_size = @opt["form-size"] || 100
    input_type = "algebraic"
    qname_0 = qname_0(qname, line_num)
    if is_matrix_type(a1)
      input_size = @opt["form-size"] || 15
      input_type = "matrix"
    end

    case mthd
    when "AlgEquiv", "CasEqualNotAsin"
      stack_mthd = mthd
      t_ans1 = cdata(a1)
      feedbk = feedback(mthd, a1)
      if mthd == "CasEqualNotAsin"
        stack_mthd = "CasEqual"
        forbidwords = ",asin,acos,atan"
      end
    when "is_same_interval",  "is_same_linear_eq", "has_same_nullspace", "has_same_deriv", "does_satisfy"
      stack_mthd = "CasEqual"
      t_ans1 = cdata(a1)
      feedbk = feedback(mthd, a1, ext)
      case mthd
      when "is_same_linear_eq"
        eq_type_check(a1, line_num)
      when "has_same_deriv"
        stack_mthd = "AlgEquiv"
      end
    when "is_same_plane"
      #        plane_type_check(a1, line_num)
      stack_mthd = "CasEqual"
      t_ans1 = cdata("transpose(matrix(" + a1 + "))")
      feedbk = feedback(mthd, a1)
      input_size = 15
      input_type = "matrix"
    when "is_same_diag"
      stack_mthd = "CasEqual"
      t_ans1 = cdata(a1)
      feedbk = feedback(mthd, a1)
      input_size = 15
      input_type = "matrix"
    else
      return nil
    end

    x.result(binding)
  end

  def txt2xml_multi(qname, qstr, a1, mthd, ext, line_num)
    qname_0 = qname_0(qname, line_num)

    case mthd
    when "multi_eigen_eq"
      case mthd
      when "multi_eigen_eq"
        desc_varnames = [["固有値", "eigenval"], ["重複度", "chofuku"], ["固有空間の次元", "jigen"]]
      else
        raise
      end
      input_size = @opt["form-size"] || 15
      x = ERB.new(TMPL_multi, nil, '-')
      ans_num, ans_dim = multi_ans_num_dim(a1)
      multi_ans_check_size(ans_dim, desc_varnames)
      ans_nodes = multi_ans_nodes(ans_num, desc_varnames, input_size)
      feedbk = multi_feedback(ans_num, desc_varnames)
      ans_forms = multi_forms(ans_num, desc_varnames)

    when "is_basis_of_same_linear_space", "is_orthonormal_basis_of_same_linear_space"
      input_size = @opt["form-size"] || 15
      x = ERB.new(TMPL_basis, nil, '-')
      basis_type_check(a1, line_num)
      dim = basis_dim(a1)
      ans_nodes = basis_ans(dim, dim, input_size)
      feedbk = basis_feedback(dim, mthd)
      ans_forms = basis_forms(dim)

    when "is_same_eigenval_and_eigenvec"
      input_size = @opt["form-size"] || 15
      x = ERB.new(TMPL_eigen, nil, '-')
      eigen_val_num, dim = eigen_num_dim(a1)
      ans_forms = eigen_forms(eigen_val_num, dim)
      feedbk = eigen_feedback(eigen_val_num, dim)
      ans_nodes = eigen_ans_nodes(eigen_val_num, dim, input_size)
    else
      return nil
    end
    x.result(binding)
  end

  def inline_tex(s)
    s.gsub(/([^\\]|\A)\$((\\\$|[^\$])*)\$/) { $1 + '\\(' + $2 + '\\)' }
  end

  def does_hold_mac
    <<EOS.chomp
stackqsimp(ex) := ratsimp( radcan( exponentialize(ex) ) );
does_hold(ex) := is( stackqsimp(ex) or ratsimp(ex) );
EOS
  end

  def feedback(mthd, a1, ext="")
    fdbk_alart = <<EOS.chomp
listofops(x) := block([], if not atom(x) then cons( op(x), flatten(map(listofops, args(x))) ) else [] );
xyalart_set : intersection({xy, yx}, setify( append(listofvars(ans1), listofops(ans1))   ));
xyalart_elem : if not emptyp( xyalart_set ) then listify(xyalart_set)[1];
xyalart : if not emptyp( xyalart_set ) then 1 else false;
sinalart : if not emptyp( intersection({sin2, sin3, sin4, sin5, cos2, cos3, cos4, cos5, tan2, tan3, tan4, tan5, asin2, asin3, acos2, acos3, atan2, atan3}, setify(listofvars(ans1))) ) then 1 else false;
fxalart_set : intersection({x, y, s, t, fx, fy, fxx, fxy, fyx, fyy}, setify(listofops(ans1)));
fxalart_elem : if not emptyp( fxalart_set ) then listify(fxalart_set)[1];
fxalart : if not emptyp( fxalart_set ) then 1 else false;
#{does_hold_mac}
ans1 : ratsubst(fxy, fyx, ans1);
EOS

    case mthd
    when "AlgEquiv", "CasEqualNotAsin"
       <<EOS.chomp
<![CDATA[
#{fdbk_alart}
a1 : #{esq_cdata(a1)};
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
    when "has_same_deriv"
      <<EOS.chomp
<![CDATA[
#{fdbk_alart}
a1 : #{esq_cdata(a1)};
a1 : diff(a1,x);
ans1 : diff(ans1, x);
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
    when "does_satisfy"
      <<EOS.chomp
<![CDATA[
#{fdbk_alart}
a1 : #{esq_cdata(a1)};
result : if does_hold( #{esq_cdata(ext)} ) then 1 else false;
]]>
EOS
    when "is_same_interval"
      <<EOS.chomp
<![CDATA[
myargs(xs) := block([as, zzz],as : if atom(xs) then xs else args(xs),if not chk_op(as, xs) then return(zzz),as);
chk_op(as, xs) := block([op1, x],if not( atom(as) ) and not( atom(xs) ) then (if member(x, as) then (op1 : op(xs),return( member(op1, ["and", "or", "<", ">", ">=", "<="]) ))),true);
edges(xs) := block([x],delete(x, flatten( scanmap(myargs, xs))));
xs_in_interval(xs, cond) := block(map(lambda([x], charfun(cond)), xs));
is_same_interval(c1, c2) := block([ret, xs1, xs2, v1, v2, x, m],ret : true,xs1 : edges(c1),xs2 : edges(c2),m : lmax( map(abs, append(xs1, xs2)) ),m : 2*min(max(m, 1), 100),ret : ret and is(xs_in_interval(xs1, c1) = xs_in_interval(xs1, c2)),ret : ret and is(xs_in_interval(xs2, c1) = xs_in_interval(xs2, c2)),if ret then (v1 : quad_qags(charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],v2 : quad_qags(charfun(c2)*charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],ret : ret and is(v1 = v2)),ret);

a1 : #{esq_cdata(a1)};
result : if is_same_interval(a1, ans1) then 1 else false;
]]>
EOS
    when "is_same_diag"
      <<EOS.chomp
<![CDATA[
is_diagonal(m) := block([col_size, row_size],col_size : length(m),row_size : length(m[1]),is(col_size = row_size) and is( m = m * diagmatrix(col_size, 1)));
get_diag_element(m) := block([len, i],len : length(m),maplist(lambda([i], m[i,i]), makelist(i, i, len)));
is_same_diag(a, x) := block([],is_diagonal(a) and is_diagonal(x) and does_hold( sort(get_diag_element(a)) = sort(get_diag_element(x)) ));
#{does_hold_mac}

a1 : #{esq_cdata(a1)};
result : if is_same_diag(a1, ans1) then 1 else false;
]]>
EOS
    when "is_same_linear_eq", "is_same_plane", "has_same_nullspace"
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
      case mthd
      when "is_same_linear_eq"
ret <<
<<EOS.chomp
a1 : #{esq_cdata(a1)};
result : if is_same_linear_eq(a1, ans1) then 1 else false;
]]>
EOS
      when "is_same_plane"
ret <<
<<EOS.chomp
a1 : #{esq_cdata(a1)};
ans1 : list_matrix_entries(ans1);
result : if is_same_plane(a1, ans1) then 1 else false;
]]>
EOS
      when "has_same_nullspace"
ret <<
<<EOS.chomp
a1 : #{esq_cdata(a1)};
result : if is_same_linear_space(args(a1), args(ans1)) then 1 else false;
]]>
EOS
      end
      ret
    else
      ""
    end
  end
  
  def validate_maxima_exp(s, line_num = 1, l = "")
    tmp = s
    until tmp == " XXX "
      prev = tmp
      tmp = tmp.gsub(/(?<=\A|[\(\[\{,]|and|or|not)\s*-?(\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*([\*\+\-\^\/\=]|[\>\<]=?))*\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*(?=\z|[\)\]\},]|and|or)/, " XXX ")
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

  def cdata(s)
    "<![CDATA[" + esq_cdata(s) + "]]>"
  end
  
  def esq_cdata(s)
    (s || "").gsub("]]>", "]]]]><![CDATA[>")
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

  def multi_ans_num_dim(s)
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

  def multi_ans_check_size(ans_dim, desc_varnames)
    unless ans_dim == desc_varnames.size
      raise "ans_dim and the size of desc_varnames are not the same"
    end
  end

  def multi_ans_nodes(ans_num, desc_varnames, input_size = 15)
    ret = ""
    (1..ans_num).each{|i|
      desc_varnames.each{|desc0, name0|
        ret << multi_val_nodes(name0, i, input_size)
      }
    }
    ret
  end

  def multi_val_nodes(name, i, input_size)
    ERB.new(<<HERE, nil, '-').result(binding)
    <input>
      <name><%= varname(name, i) %></name>
      <type>algebraic</type>
      <tans>1</tans>
      <boxsize><%= input_size %></boxsize>
      <strictsyntax>1</strictsyntax>
      <insertstars>0</insertstars>
      <syntaxhint></syntaxhint>
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]] </forbidwords>
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

  def varname(name, idx)
    "#{name}_#{idx}"
  end

  def multi_forms(ans_num, desc_varnames)
    ERB.new(<<HERE, nil, '-').result(binding)
<% (1..ans_num).each do |idx| %>
<p>
<%     desc_varnames.each do |desc0, name0| -%>
<%=h desc0  %> [[input:<%= varname(name0, idx) %>]] &nbsp;&nbsp;&nbsp;
<%     end -%>
</p>
<div>
<%     desc_varnames.each do |desc0, name0| -%>
[[validation:<%= varname(name0, idx) %>]]
<%     end -%>
</div>
<br><br>
<% end -%>
HERE
  end

  def multi_feedback(ans_num, desc_varnames)
    ERB.new(<<HERE, nil, '-').result(binding).chomp
<![CDATA[
#{does_hold_mac}
sans1 : stackqsimp([<%= (1..ans_num).map{|idx| "[" + desc_varnames.map{|desc0, name0| varname(name0, idx) }.join(", ") + "]" }.join(",") %>]);
ith : 0;
result : is(<%= ans_num %> = length(unique(sans1)));
<% (1..ans_num).each do |idx| -%>
ith : if result then ith + 1 else ith;
sans0 : [<%= desc_varnames.map{|desc0, name0| varname(name0, idx) }.join(", ") %>];
result : result and some(lambda([x], does_hold(sans0 = x)), k1);
<% end -%>
]]>
HERE
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

  def n_join(n, str, sp = ", ")
    (1..n).map{|i| str % i }.join(sp)
  end

  def eigen_feedback(eigen_val_num, dim)
    ans_vals = n_join(eigen_val_num, "ans_val%d") # ans_val == ans_eigenval
    large_Ns = n_join(dim, "N", ", ")
    ERB.new(<<HERE, nil, '-').result(binding).chomp
<![CDATA[
<%= basis_feedback_lib_mac() %>
ith : 0;
result : is(<%= eigen_val_num %> = length(unique([<%= ans_vals %>])));
ith : if result then ith + 1 else ith;
<%- (1..eigen_val_num).each do |i| -%>
vec<%= i %> : delete([<%= large_Ns %>], maplist(list_matrix_entries, [<%= n_join(dim, "ans%d_%%d" % i) %>]));
kvec<%= i %> : assoc(ans_val<%= i %>, k1);
result : result and listp(kvec<%= i %>) and is_basis(vec<%= i %>) and is_same_linear_space(kvec<%= i %>, vec<%= i %>);
ith : if result then ith + 1 else ith;
<%- end -%>
]]>
HERE
  end

  def eigen_val_nodes(i)
    ERB.new(<<HERE, nil, '-').result(binding)
    <input>
      <name>ans_val<%= i %></name>
      <type>algebraic</type>
      <tans>1</tans>
      <boxsize>15</boxsize>
      <strictsyntax>1</strictsyntax>
      <insertstars>0</insertstars>
      <syntaxhint></syntaxhint>
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]] </forbidwords>
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

  def eigen_ans_nodes(eigen_val_num, dim, input_size)
    ret = ""
    (1..eigen_val_num).each{|i|
      ret << eigen_val_nodes(i)
      ret << basis_ans(dim, dim, input_size, "#{i}_")
    }
    ret
  end

  def eigen_forms(eigen_val_num, dim)
    ret = ""
    (1..eigen_val_num).each{|idx|
      ans = n_join(dim, "[[input:ans#{idx}_%d]]", " ")
      valid = "[[validation:ans_val#{idx}]] "
      valid += n_join(dim, "[[validation:ans#{idx}_%d]]", " ")
      ret << ERB.new(<<HERE, nil, '-').result(binding)
<p> 固有値 [[input:ans_val<%= idx %>]] それに対する固有ベクトルは次のベクトルの1次結合である <%= ans %></p>
<div><%= valid %></div><br><br>
HERE
    }
    ret
  end

  def basis_dim(s)
    if m = s.match(/\[([^\[\]]*?)\]/)
      $1.split(",").size
    end
  end

  def basis_forms(dim)
    ret = ERB.new(<<HERE, nil, '-').result(binding).chomp
<p> <%= basis_ans_form(dim) %></p>
<div><%= basis_validation_form(dim) %></div>
HERE
  end

  def basis_ans_form(dim)
    n_join(dim, "[[input:ans%d]]", " ")
  end

  def basis_validation_form(dim)
    n_join(dim, "[[validation:ans%d]]", " ")
  end

  def basis_ans(n, dim, input_size, prefix="")
    ERB.new(<<HERE, nil, '-').result(binding)
<%- (1..n).each do |i| -%>
    <input>
      <name>ans<%= prefix %><%= i %></name>
      <type>matrix</type>
      <tans>matrix(<%= n_join(dim, "[1]", ",")  %>)</tans>
      <boxsize><%= input_size %></boxsize>
      <strictsyntax>1</strictsyntax>
      <insertstars>0</insertstars>
      <syntaxhint></syntaxhint>
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]] </forbidwords>
      <allowwords></allowwords>
      <forbidfloat>1</forbidfloat>
      <requirelowestterms>0</requirelowestterms>
      <checkanswertype>0</checkanswertype>
      <mustverify>1</mustverify>
      <showvalidation>1</showvalidation>
      <options></options>
    </input>
<%- end -%>
HERE
  end
  
  def basis_feedback_lib_mac
<<HERE.chomp
#{does_hold_mac}
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(radcan(a)),x0 : listify(radcan(x)),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
is_basis(x) := block([xm],xm : apply(matrix, x),is( rank(xm) = length(x) ));
is_orthonormal_basis(x) := block([xm],xm : apply(matrix, radcan(x)),does_hold( ident(length(x)) = xm.(conjugate(transpose(xm))) ));
HERE
  end

  def basis_feedback(dim, mthd)
    b1 = n_join(dim, "list_matrix_entries(ans%d)", ", ") # b1 == ans1 == student's answer
    large_Ns = n_join(dim, "N", ", ")
    basis_chk =
      case mthd
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
end


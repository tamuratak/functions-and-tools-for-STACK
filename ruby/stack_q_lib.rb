# -*- coding: utf-8 -*-
require 'erb'

class STACK_Q
  include ERB::Util

  def initialize(s)
    @txt = s
    @err_msg = ""
  end
  attr_reader :err_msg

  def txt2xml
    ret = ""
    ret << HEAD
    line_num = 1

    @txt.each_line{|l|
      next if /\A\s*\Z/ =~ l

      x = ERB.new(TMPL)
      input_size = 55
      input_type = "algebraic"

      qname, qstr, a1, mthd, ext = l.split(/\s*\*\*\s*/).map{|s| s.sub(/\A\s*/, "").sub(/\s*\Z/, "") }
      mthd = mthd || "AlgEquiv"
      forbidwords = ""

      begin
        validate_maxima_exp(a1)
      rescue RuntimeError
        raise "\n\n" + "validation error of maxima expression at line #{line_num}" + "\n" + l + "\n\n"
      end

      if is_matrix_type(a1)
        input_size = 10
        input_type = "matrix"
      end

      qstr = '\(\,\)  ' + inline_tex(qstr)

      # teacher's answer == a1 == t_ans1 == prt_ans1, (prt stands for potential response tree)
      # student's answer == ans1
      case mthd
      when "AlgEquiv", "CasEqual", "CasEqualNotAsin"
        stack_mthd = mthd
        t_ans1 = cdata(a1)
        prt_ans1 = cdata(a1)
        feedbk = ""
        if mthd == "CasEqualNotAsin"
          stack_mthd = "CasEqual"
          forbidwords = ",asin,acos,atan"
        end
      when "is_same_interval",  "is_same_linear_eq", "is_same_tri", "has_same_deriv", "does_satisfy"
        stack_mthd = "CasEqual"
        t_ans1 = cdata(a1)
        prt_ans1 = "a1"
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
        prt_ans1 = "a1"
        feedbk = feedback(mthd, a1)
        input_size = 15
        input_type = "matrix"
      when "is_basis_of_same_linear_space"
        x = ERB.new(TMPL2)
        basis_type_check(a1, line_num)
        dim = basis_dim(a1)
        inputs = basis_ans(dim, dim)
        prt = basis_prt(dim)
        feedbk = basis_feedback(dim)
        basis_ans_form0 = basis_ans_form(dim)
        basis_validation_form0 = basis_validation_form(dim)
      else
        @err_msg = "error at line: #{line_num}"
        raise "invalid grading method"
      end

      ret << x.result(binding)
      line_num  += 1
    }
    
    ret << FOOT
    
  end

  def inline_tex(s)
    s.gsub(/([^\\]|\A)\$((\\\$|[^\$])*)\$/) { $1 + '\\(' + $2 + '\\)' }
  end

  def feedback(mthd, a1, ext="")
    case mthd
    when "has_same_deriv"
      <<EOS.chop
<![CDATA[
a1 : #{esq_cdata(a1)};
a1 : diff(a1,x);
ans1 : diff(ans1, x);
]]>
EOS
    when "is_same_tri"
      <<EOS.chop
<![CDATA[
echelon_1(m) := block([arry, m0, len, k, i, j],m0 : echelon(m),len : length(m),(for i: 2 while i <= len do (arry : sublist_indices(m0[i], lambda([x], x = 1)),(if not is(arry = []) then (k : arry[1],(for j: 1 while j < i do ((m0[j] : m0[j] - m0[j][k] * m0[i]))))))),m0);
is_triangle(m) := block([len,i,k0,k,arry, ret],ret : true,len : length(m),len0 : length(m[1]) + 1,arry : sublist_indices(m[1], lambda([x], not (x = 0)) ),(if is(arry = []) then (k0 : length(m[1]) + 1) else (k0 : arry[1])),(for i: 2 while i <= len do (arry : sublist_indices(m[i], lambda([x], not (x = 0)) ),k : (if is(arry = []) then (length(m[1]) + 1) else (arry[1])),(if not is( (k > k0) or (len0 = k and len0 = k0) ) then (ret : ret and false)),k0 : k)),ret);
is_same_triangle(a, x) := block([],a0 : echelon_1(a),x0 : echelon_1(x),x1 : triangularize(x),is(is_triangle(x) and (a0 = x0)));
a1 : #{esq_cdata(a1)};
a1 : if is_same_triangle(a1, ans1) then ans1 else false;
]]>
EOS
    when "does_satisfy"
      <<EOS.chop
<![CDATA[
a1 : #{esq_cdata(a1)};
a1 : if is(ratsimp(#{esq_cdata(ext)})) then ans1 else false;
]]>
EOS
    when "is_same_interval"
      <<EOS.chop
<![CDATA[
myargs(xs) := block([as, zzz],as : if atom(xs) then xs else args(xs),if not chk_op(as, xs) then return(zzz),as);
chk_op(as, xs) := block([op1, x],if not( atom(as) ) and not( atom(xs) ) then (if member(x, as) then (op1 : op(xs),return( member(op1, ["and", "or", "<", ">", ">=", "<="]) ))),true);
edges(xs) := block([x],delete(x, flatten( scanmap(myargs, xs))));
xs_in_interval(xs, cond) := block(map(lambda([x], charfun(cond)), xs));
is_same_interval(c1, c2) := block([ret, xs1, xs2, v1, v2, x, m],ret : true,xs1 : edges(c1),xs2 : edges(c2),m : lmax( map(abs, append(xs1, xs2)) ),m : 2*min(max(m, 1), 100),ret : ret and is(xs_in_interval(xs1, c1) = xs_in_interval(xs1, c2)),ret : ret and is(xs_in_interval(xs2, c1) = xs_in_interval(xs2, c2)),if ret then (v1 : quad_qags(charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],v2 : quad_qags(charfun(c2)*charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],ret : ret and is(v1 = v2)),ret);

a1 : #{esq_cdata(a1)};
a1 : if is_same_interval(a1, ans1) then ans1 else false;
]]>
EOS
    when "is_same_linear_eq", "is_same_plane"
      ret = ""
      ret <<
<<EOS
<![CDATA[
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(a),x0 : listify(x),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret); 
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
<<EOS.chop
a1 : #{esq_cdata(a1)};
a1 : if is_same_linear_eq(a1, ans1) then ans1 else false;
]]>
EOS
      when "is_same_plane"
ret <<
<<EOS.chop
a1 : #{esq_cdata(a1)};
ans1 : list_matrix_entries(ans1);
a1 : if is_same_plane(a1, ans1) then ans1 else false;
]]>
EOS
      end
      ret
    else
      ""
    end
  end
  
  def validate_maxima_exp(s)
    tmp = s
    until tmp == " XXX "
      prev = tmp
      tmp = tmp.gsub(/(?<=\A|[\(\[\{,]|and|or|not)\s*-?(\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*[\*\+\-\^\/\=\>\<])*\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*(?=\z|[\)\]\},]|and|or)/, " XXX ")
      tmp = tmp.gsub(/(?!(and|or|not)\s*\()([a-z]{3,})\s*\(( XXX ,)* XXX \)/, " XXX ")
      tmp = tmp.gsub(/\( XXX \)/, " XXX ")
      tmp = tmp.gsub(/\[( XXX ,)* XXX \]/, " XXX ")
      tmp = tmp.gsub(/\{( XXX ,)* XXX \}/, " XXX ")
      tmp = tmp.gsub(/ XXX (and|or) XXX /, " XXX ")
      tmp = tmp.gsub(/not XXX/, " XXX ")
      if tmp == prev
        raise s
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

  def basis_dim(s)
    if m = s.match(/\[([^\[\]]*?)\]/)
      $1.split(",").size
    end
  end

  def basis_ans_form(dim)
    (1..dim).map{|i| "[[input:ans#{i}]]"}.join(" ") 
  end

  def basis_validation_form(dim)
    (1..dim).map{|i| "[[validation:ans#{i}]]"}.join(" ") 
  end

  def basis_ans(n, dim)
    ERB.new(<<HERE, nil, '-').result(binding)
<%- (1..n).each do |i| -%>
    <input>
      <name>ans<%= i %></name>
      <type>matrix</type>
      <tans>matrix(<%= (["[1]"]*dim).join(",")  %>)</tans>
      <boxsize>5</boxsize>
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

  def basis_prt(n)
    n1 = n-1
    ERB.new(<<HERE, nil, '-').result(binding)
<%- (0..(n1)).each do |i| -%>
      <node>
        <name><%= i %></name>
        <answertest>CasEqual</answertest>
        <sans>ans<%= i+1 %></sans>
        <tans>a<%= i+1 %></tans>
        <testoptions></testoptions>
        <quiet>0</quiet>
        <truescoremode>+</truescoremode>
        <truescore><%= i == n1 ? 1.0 : 0.0 %></truescore>
        <truepenalty></truepenalty>
        <truenextnode><%= i == n1 ? -1 : i+1 %></truenextnode>
        <trueanswernote>prt1-<%= i+1 %>-T</trueanswernote>
        <truefeedback format="html">
          <text></text>
        </truefeedback>
        <falsescoremode>=</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>-1</falsenextnode>
        <falseanswernote>prt1-<%= i+1 %>-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
<%- end -%>
HERE
  end
  
  def basis_feedback(dim)
    b1 = (1..dim).map{|i| "list_matrix_entries(ans#{i})"}.join(", ")
    alhs = (1..dim).map{|i| "a#{i}" }.join(", ")
    arhs = (1..dim).map{|i| "ans#{i}" }.join(", ")
    zeros = (1..dim).map{|i| "0" }.join(", ")
<<"HERE"
<![CDATA[is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(a),x0 : listify(x),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
is_basis(x) := block([ret, x0, xm, i, n], ret : true, x0 : x, xm : apply(matrix, x0), ret: true, n : -(length(x0)+1), for i:1 thru length(x0) do (m : apply(matrix, append(rest(x0,i), rest(x0,n+i))), ret : ret and is(rank(m) + 1 = rank(xm))), ret) ;
b1 : delete([#{zeros}], [#{b1}]);
x : if is_same_linear_space(k1, b1) and is_basis(b1) then ([#{alhs}] : [#{arhs}]) else false;]]>
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


class STACK_Q
  include ERB::Util
  HEAD = <<"EOS"
<?xml version="1.0" encoding="UTF-8"?>
<quiz>
<!-- question: 0  -->
  <question type="category">
    <category>
        <text>$course$/stack_q</text>

    </category>
  </question>
EOS

  TMPL = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname  %></text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p><%=h qname  %></p>
<p><%=h qstr  %> [[input:ans1]]</p>
<div>[[validation:ans1]]</div>]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.0000000</penalty>
    <hidden>0</hidden>
    <questionvariables>
      <text></text>
    </questionvariables>
    <specificfeedback format="html">
      <text><![CDATA[<p>[[feedback:prt1]]</p>]]></text>
    </specificfeedback>
    <questionnote>
      <text></text>
    </questionnote>
    <questionsimplify>1</questionsimplify>
    <assumepositive>0</assumepositive>
    <prtcorrect format="html">
      <text><![CDATA[<p>よくできました。正解です!</p>]]></text>
    </prtcorrect>
    <prtpartiallycorrect format="html">
      <text><![CDATA[<p>惜しい！部分的に正解です。</p>]]></text>
    </prtpartiallycorrect>
    <prtincorrect format="html">
      <text><![CDATA[<p>残念，間違いです。</p>]]></text>
    </prtincorrect>
    <multiplicationsign>dot</multiplicationsign>
    <sqrtsign>1</sqrtsign>
    <complexno>i</complexno>
    <inversetrig>cos-1</inversetrig>
    <matrixparens>[</matrixparens>
    <variantsselectionseed></variantsselectionseed>
    <input>
      <name>ans1</name>
      <type><%= input_type %></type>
      <tans><%= t_ans1 %></tans>
      <boxsize><%= input_size %></boxsize>
      <strictsyntax>1</strictsyntax>
      <insertstars>0</insertstars>
      <syntaxhint></syntaxhint>
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]]<%=h forbidwords %> </forbidwords>
      <allowwords></allowwords>
      <forbidfloat>1</forbidfloat>
      <requirelowestterms>0</requirelowestterms>
      <checkanswertype>0</checkanswertype>
      <mustverify>1</mustverify>
      <showvalidation>1</showvalidation>
      <options></options>
    </input>
    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><%= feedbk %></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest><%=h stack_mthd %></answertest>
        <sans>ans1</sans>
        <tans><%= prt_ans1 %></tans>
        <testoptions></testoptions>
        <quiet>0</quiet>
        <truescoremode>=</truescoremode>
        <truescore>1.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-1-T</trueanswernote>
        <truefeedback format="html">
          <text></text>
        </truefeedback>
        <falsescoremode>=</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>-1</falsenextnode>
        <falseanswernote>prt1-1-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
    </prt>
  </question>
EOS

  TMPL2 = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname  %></text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p><%=h qname  %></p>
<p><%=h qstr  %> <p> <%=h basis_ans_form0 %></p>
<div><%= basis_validation_form0 %></div>]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.0000000</penalty>
    <hidden>0</hidden>
    <questionvariables>
      <text>k1 : <%=esq_cdata a1 %>;</text>
    </questionvariables>
    <specificfeedback format="html">
      <text><![CDATA[<p>[[feedback:prt1]]</p>]]></text>
    </specificfeedback>
    <questionnote>
      <text></text>
    </questionnote>
    <questionsimplify>1</questionsimplify>
    <assumepositive>0</assumepositive>
    <prtcorrect format="html">
      <text><![CDATA[<p>よくできました。正解です!</p>]]></text>
    </prtcorrect>
    <prtpartiallycorrect format="html">
      <text><![CDATA[<p>惜しい！部分的に正解です。</p>]]></text>
    </prtpartiallycorrect>
    <prtincorrect format="html">
      <text><![CDATA[<p>残念，間違いです。</p>]]></text>
    </prtincorrect>
    <multiplicationsign>dot</multiplicationsign>
    <sqrtsign>1</sqrtsign>
    <complexno>i</complexno>
    <inversetrig>cos-1</inversetrig>
    <matrixparens>[</matrixparens>
    <variantsselectionseed></variantsselectionseed>
<%= inputs %>
    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><%= feedbk %></text>
      </feedbackvariables>
<%= prt %>
    </prt>
  </question>
EOS

  FOOT = "</quiz>"
end

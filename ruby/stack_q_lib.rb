# -*- coding: utf-8 -*-

require 'erb'

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
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]] </forbidwords>
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

  FOOT = "</quiz>"

  def initialize(s)
    @txt = s
    @err_msg = ""
  end
  attr_reader :err_msg

  def txt2xml
    ret = ""
    ret << HEAD
    x = ERB.new(TMPL)
    
    n = 1
    input_size = 55
    input_type = "algebraic"
    @txt.each_line{|l|
      next if /\A\s*\Z/ =~ l
      qname, qstr, ans1, mthd = l.split(/\s*\*\*\s*/).map{|s| s.sub(/\A\s*/, "").sub(/\s*\Z/, "") }    
      mthd = mthd || "AlgEquiv"
      case mthd
      when "AlgEquiv", "CasEqual"
        stack_mthd = mthd
        t_ans1 = cdata(ans1)
        prt_ans1 = cdata(ans1)
        feedbk = ""
      when "is_same_interval", "is_same_linear_space", "is_same_plane", "is_same_linear_eq"
        stack_mthd = "CasEqual"
        t_ans1 = cdata(ans1)
        prt_ans1 = "a1"
        feedbk = feedback(mthd, ans1)
      else
        @err_msg = "error at line: #{n}"
        raise
      end
      if is_matrix_type(ans1)
        input_size = 4
        input_type = "matrix"
      end
      ret << x.result(binding)
      n += 1
    }
    
    ret << FOOT

  end

  def feedback(mthd, ans1)
    case mthd
    when "is_same_interval"
      <<EOS.chop
<![CDATA[myargs(xs) := block([as, zzz],as : if atom(xs) then xs else args(xs),if not chk_op(as, xs) then return(zzz),as);
chk_op(as, xs) := block([op1, x],if not( atom(as) ) and not( atom(xs) ) then (if member(x, as) then (op1 : op(xs),return( member(op1, ["and", "or", "<", ">", ">=", "<="]) ))),true);
edges(xs) := block([x],delete(x, flatten( scanmap(myargs, xs))));
xs_in_interval(xs, cond) := block(map(lambda([x], charfun(cond)), xs));
is_same_interval(c1, c2) := block([ret, xs1, xs2, v1, v2, x, m],ret : true,xs1 : edges(c1),xs2 : edges(c2),m : lmax( map(abs, append(xs1, xs2)) ),m : 2*min(max(m, 1), 100),ret : ret and is(xs_in_interval(xs1, c1) = xs_in_interval(xs1, c2)),ret : ret and is(xs_in_interval(xs2, c1) = xs_in_interval(xs2, c2)),if ret then (v1 : quad_qags(charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],v2 : quad_qags(charfun(c2)*charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],ret : ret and is(v1 = v2)),ret);

a1 : #{esq_cdata(ans1)};
a1 : if is_same_interval(a1, ans1) then ans1 else false;]]>
EOS
    when "is_same_linear_space", "is_same_plane", "is_same_linear_eq"
<<EOS.chop
<![CDATA[is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(a),x0 : listify(x),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret); 
basis_of_plane(v) := block([params],params : listofvars(v),map(lambda([v1], diff(v, v1)), params));
pos_of_plane(v) := block([v0 : v, params, i],params : listofvars(v),for i:1 thru length(params) do v0 : subst(0, params[i], v0),v0);
is_same_plane(v1, v2) := block([b1, b2, p1, p2, ret : true],b1 : basis_of_plane(v1),b2 : basis_of_plane(v2),ret : ret and is_same_linear_space(b1, b2),p1 : pos_of_plane(v1),p2 : pos_of_plane(v2),ret : ret and is(p1 = p2)); 
eq_to_param(eq) := block([params, tmp],eq : listify(eq),params : listofvars(eq),tmp : solve(eq, params),subst(tmp[1], params));
is_same_linear_eq(eq1, eq2) := block([pa1, pa2],pa1 : eq_to_param(eq1),pa2 : eq_to_param(eq2),is_same_plane(pa1, pa2));

a1 : #{esq_cdata(ans1)};
a1 : if #{mthd}(a1, ans1) then ans1 else false;]]>
EOS
    else
      ""
    end
  end

  def cdata(s)
    "<![CDATA[" + esq_cdata(s) + "]]>"
  end
  
  def esq_cdata(s)
    (s || "").gsub("]]>", "]]]]><![CDATA[>")
  end

  def is_matrix_type(a)
    if /\Amatrix/ =~ a
      7.times{
        a = a.gsub(/\([^\(\)]*\)/, "")
      }
      "matrix" == a
    else
      false
    end
  end

end


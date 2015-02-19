# -*- coding: utf-8 -*-

require 'erb'

module STACK_Q
  extend ERB::Util
  HEAD = <<"EOS" 
<?xml version="1.0" encoding="UTF-8"?>
<quiz>
<!-- question: 0  -->
  <question type="category">
    <category>
        <text>$course$/myqb</text>

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
      <type>algebraic</type>
      <tans><%= t_ans1 %></tans>
      <boxsize>55</boxsize>
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
        <answertest><%= stack_mthd %></answertest>
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

  def self.txt2xml(txt)    
    ret = ""
    ret << HEAD
    x = ERB.new(TMPL)
    
    n = 1
    txt.each_line{|l|
      next if /\A\s*\Z/ =~ l
      qname, qstr, ans1, mthd = l.split(/\s*\*\*\s*/)    
      mthd = mthd || "AlgEquiv"
      mthd = mthd.gsub(/\s*/, "")
      case mthd
      when "AlgEquiv", "CasEqual"
        stack_mthd = mthd
        t_ans1 = cdata(ans1)
        prt_ans1 = cdata(ans1)
        feedbk = ""
      when "is_same_interval"
        stack_mthd = "CasEqual"
        t_ans1 = cdata(ans1)
        prt_ans1 = "a1"
        feedbk = feedbak(mthd, ans1)
      else
        raise "line:#{n} compare method #{mthd} is not appropriate."
      end
      ret << x.result(binding)
      n += 1
    }
    
    ret << FOOT
  end

  def self.feedbak(mthd, ans1)
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
    else
      ""
    end
  end

  def self.cdata(s)
    "<![CDATA[" + esq_cdata(s) + "]]>"
  end
  
  def self.esq_cdata(s)
    (s || "").gsub("]]>", "]]]]><![CDATA[>")
  end

end


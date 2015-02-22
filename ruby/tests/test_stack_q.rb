# -*- coding: utf-8 -*-
require 'test/unit'

$LOAD_PATH.push File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'stack_q_lib'

class TestStackQ < Test::Unit::TestCase
  Kekka01 = <<EOS.chop
<?xml version="1.0" encoding="UTF-8"?>
<quiz>
<!-- question: 0  -->
  <question type="category">
    <category>
        <text>$course$/stack_q</text>

    </category>
  </question>
  <question type="stack">
    <name>
      <text>abcd01</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p>abcd01</p>
<p>abcd02 [[input:ans1]]</p>
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
      <tans><![CDATA[abcd03]]></tans>
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
        <text></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest>AlgEquiv</answertest>
        <sans>ans1</sans>
        <tans><![CDATA[abcd03]]></tans>
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
</quiz>
EOS

  Func01 = <<EOS.chop
<![CDATA[myargs(xs) := block([as, zzz],as : if atom(xs) then xs else args(xs),if not chk_op(as, xs) then return(zzz),as);
chk_op(as, xs) := block([op1, x],if not( atom(as) ) and not( atom(xs) ) then (if member(x, as) then (op1 : op(xs),return( member(op1, ["and", "or", "<", ">", ">=", "<="]) ))),true);
edges(xs) := block([x],delete(x, flatten( scanmap(myargs, xs))));
xs_in_interval(xs, cond) := block(map(lambda([x], charfun(cond)), xs));
is_same_interval(c1, c2) := block([ret, xs1, xs2, v1, v2, x, m],ret : true,xs1 : edges(c1),xs2 : edges(c2),m : lmax( map(abs, append(xs1, xs2)) ),m : 2*min(max(m, 1), 100),ret : ret and is(xs_in_interval(xs1, c1) = xs_in_interval(xs1, c2)),ret : ret and is(xs_in_interval(xs2, c1) = xs_in_interval(xs2, c2)),if ret then (v1 : quad_qags(charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],v2 : quad_qags(charfun(c2)*charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],ret : ret and is(v1 = v2)),ret);

a1 : abcd;
a1 : if is_same_interval(a1, ans1) then ans1 else false;]]>
EOS

  def setup
    @stck = STACK_Q.new("")
  end

  def test_s
    assert_equal( Kekka01, STACK_Q.new("abcd01**abcd02**abcd03").txt2xml )
    assert_equal( Kekka01, STACK_Q.new(" abcd01 ** abcd02 ** abcd03 ").txt2xml )
    assert_equal( Kekka01.gsub("abcd01", "&lt;"),
                  STACK_Q.new(" < ** abcd02 ** abcd03 ").txt2xml )
    
    assert_equal( Kekka01, 
                  STACK_Q.new("abcd01**abcd02**abcd03**AlgEquiv").txt2xml )
    assert_equal( Kekka01.gsub(/AlgEquiv/, "CasEqual"), 
                  STACK_Q.new("abcd01**abcd02**abcd03**CasEqual").txt2xml )
  end

  def test_f
    assert_equal( Func01, 
                  @stck.feedback('is_same_interval', 'abcd') )
    assert_equal( Func01.gsub("abcd", "]]]]><![CDATA[>"), 
                  @stck.feedback('is_same_interval', ']]>') )
  end

  def test_m
    assert_equal( true,
                  @stck.is_matrix_type("matrix((),() )") )
    assert_equal( false,
                  @stck.is_matrix_type("matrix((),() ) + matrix()") )

    assert_equal( Kekka01.gsub("algebraic", "matrix").gsub("55", "4").gsub("abcd03", "matrix([1],[2],[3])"), 
                  STACK_Q.new("abcd01**abcd02**matrix([1],[2],[3])").txt2xml )

    assert_equal( Kekka01.gsub("abcd03", "matrix([1],[2],[3])+matrix([1],[2],[3])"), 
                  STACK_Q.new("abcd01**abcd02**matrix([1],[2],[3])+matrix([1],[2],[3])").txt2xml )
  end

  def test_e
    assert_raise(ArgumentError){ STACK_Q.new("a**b**\xf1\xf1").txt2xml }
  end
end

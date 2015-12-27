# -*- coding: utf-8 -*-
require 'test/unit'

$LOAD_PATH.push File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'stack_q_lib'

class TestStackQ < Test::Unit::TestCase

  def setup
    @stck = STACK_Q.new("")
  end

  def test_whole_xml
    dir = File.dirname( File.expand_path(__FILE__) )
    e_stk = File.read( File.join(dir, "e.stk") )
    e_xml = File.read( File.join(dir, "e.xml") ).chop
    assert_equal(e_xml, STACK_Q.new(e_stk).txt2xml)
  end

  def test_feedback
    assert_equal( Feedbk01.gsub("ZZZ", "abcd03"), @stck.feedback("AlgEquiv", "abcd03") )

    assert_equal( Feedbk02,
                  @stck.feedback('is_same_interval', 'abcd') )
    assert_equal( Feedbk02.gsub("abcd", "]]]]><![CDATA[>"),
                  @stck.feedback('is_same_interval', ']]>') )
  end

   def test_m
     assert_equal( true,
                   @stck.is_matrix_type("matrix((),() )") )
      assert_equal( true,
                   @stck.is_matrix_type("matrix ((),() )") )
     assert_equal( false,
                   @stck.is_matrix_type("matrix((),() ) + matrix()") )
   end

  def test_e
    assert_raise(ArgumentError){ STACK_Q.new("a**b**\xf1\xf1").txt2xml }
  end

  def test_matrix_x
    assert_equal(3, @stck.basis_dim("[[1,1,1], [2,1,1], [3,1,1]]"))
    assert_equal(3, @stck.basis_dim("[[1,1,1], [2,1,1]]"))
    assert_equal(2, @stck.basis_dim("[[1,1], [2,1]]"))
    assert_equal( Kekka02,
                  STACK_Q.new("abs ** xyz ** [[1,1,0], [1,0,0]] ** is_basis_of_same_linear_space").txt2xml )
  end
  
  def test_multi
    assert_equal([2,2], @stck.multi_ans_num_dim("[[1,2],[3,4]]"))
    assert_equal([2,3], @stck.multi_ans_num_dim("[[1,2,3],[3,3,4]]"))
    assert_raise(RuntimeError){ @stck.multi_ans_check_size(2, [["desc", "ans"]]) }
    assert_raise(RuntimeError){ @stck.multi_ans_check_size(3, [["desc", "ans"],["desc1", "ans1"]]) }
    assert_nothing_raised{ @stck.multi_ans_check_size(1, [["desc", "ans"]]) }
  end

  def test_eigen
    assert_equal( [1, 2], @stck.eigen_num_dim("[[1, [[1,2]]]]") )
    assert_equal( [1, 2], @stck.eigen_num_dim("[[1, [[1,2], [1,1]]]]") )
    assert_raise(RuntimeError){ @stck.eigen_num_dim("[ [1, [[1,2], [1]]] ]" ) }
    assert_raise(RuntimeError){ @stck.eigen_num_dim("[ [1, [[1,2], [1,1]]], [2, [[1]]] ]" ) }
    assert_equal( [2, 2], @stck.eigen_num_dim("[[1, [[1,2]]], [2, [[1,0]]]]") )
    assert_equal( [2, 3], @stck.eigen_num_dim("[[1, [[1,2,1]]], [2, [[1,0,0]]]]") )
  end

  def test_basis_type_check
    assert_nothing_raised{ 
      @stck.basis_type_check("[[1,1]]", 1)
      @stck.basis_type_check("[[1,1],[1,2]]", 1)
    }
    assert_raise(RuntimeError) {
      @stck.basis_type_check("[[1,1],[1]]", 1)
    }
    assert_raise(RuntimeError) {
      @stck.basis_type_check("{[1,1],[1,0]}", 1)
    }
  end

  def test_plane_type_check
    assert_nothing_raised{
      @stck.plane_type_check("[t,s,0]", 1)
    }
    assert_raise(RuntimeError) {
      @stck.plane_type_check("{1,1}", 1)
    }
  end

  def test_eq_type_check
    assert_nothing_raised{
      @stck.eq_type_check("{x,y,z}", 1)
    }
    assert_raise(RuntimeError) {
      @stck.eq_type_check("[x,y]", 1)
    }
  end

  def test_inline_tex
    assert_equal('\\(abc\\)', @stck.inline_tex('$abc$'))
    assert_not_equal('\\(abc\\)', @stck.inline_tex('$abc'))
    assert_equal('\(\sin\)', @stck.inline_tex('$\sin$'))
    assert_equal(' \(\$\)', @stck.inline_tex(' $\$$'))
    assert_equal('\$\$$', @stck.inline_tex('\$\$$'))

    assert_equal('\(abc\) \(xyz\)', @stck.inline_tex('$abc$ $xyz$'))
  end

  def test_validate_maxima_exp
    assert_nothing_raised { @stck.validate_maxima_exp("%e") }
    assert_nothing_raised { @stck.validate_maxima_exp("%pi") }
    assert_nothing_raised { @stck.validate_maxima_exp("%i") }
    assert_nothing_raised { @stck.validate_maxima_exp("a") }
    assert_nothing_raised { @stck.validate_maxima_exp("-a") }
    assert_nothing_raised { @stck.validate_maxima_exp("a*b") }
    assert_nothing_raised { @stck.validate_maxima_exp("2*a*b") }
    assert_nothing_raised { @stck.validate_maxima_exp("2*a*b + c") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("a b") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("2a")  }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("sin x")  }

    assert_nothing_raised { @stck.validate_maxima_exp("log (sin(x*y))") }
    assert_nothing_raised { @stck.validate_maxima_exp("sin(x)") }
    assert_nothing_raised { @stck.validate_maxima_exp("2 * (a + sin(x))") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("2 * (a + sin(x)) (b + c)") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("2 * (a + 2sin(x))") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("2 (a + sin(x))") }
    assert_nothing_raised { @stck.validate_maxima_exp("log(1 - x)") }
    assert_nothing_raised { @stck.validate_maxima_exp("(a + sin(x)) / (b + c)") }
    assert_nothing_raised { @stck.validate_maxima_exp("sin(x) - a*b") }
    assert_nothing_raised { @stck.validate_maxima_exp("(sin(-x) - (-a*b))") }

    assert_nothing_raised { @stck.validate_maxima_exp("[1, 2, 3]") }
    assert_nothing_raised { @stck.validate_maxima_exp("matrix([1,2], [3,4])") }
    assert_nothing_raised { @stck.validate_maxima_exp("sqrt(23)*atan(2*x/sqrt(12)) + 21/2*log(12 + x^12)") }
    assert_nothing_raised { @stck.validate_maxima_exp("-1 - 3*y + z") }
    assert_nothing_raised { @stck.validate_maxima_exp("-1 - 3*y + z = 2*x") }
    assert_nothing_raised { @stck.validate_maxima_exp("{-1 - 3*y + z = 2*x}") }

    assert_nothing_raised { @stck.validate_maxima_exp("1 < 2 and 2 < 3") }
    assert_nothing_raised { @stck.validate_maxima_exp("1 < 2 and not 2 < 3") }
    assert_nothing_raised { @stck.validate_maxima_exp("1 < 2 and (2 < 3 or 1 = 1)") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("1 not (1)") }
    assert_raise(RuntimeError) { @stck.validate_maxima_exp("1 not and 1") }

    assert_nothing_raised { @stck.validate_maxima_exp("(-((2*x)/(-3 + x^3)) - log(2 - 3*x) + log(2 + 4*x))") }
  end

end 


class TestStackQ  < Test::Unit::TestCase

  Feedbk01 = <<EOS.chop
<![CDATA[
listofops(x) := block([], if not atom(x) then cons( op(x), flatten(map(listofops, args(x))) ) else [] );
xyalart_set : intersection({xy, yx}, setify( append(listofvars(ans1), listofops(ans1))   ));
xyalart_elem : if not emptyp( xyalart_set ) then listify(xyalart_set)[1];
xyalart : if not emptyp( xyalart_set ) then 1 else false;
sinalart : if not emptyp( intersection({sin2, sin3, sin4, sin5, cos2, cos3, cos4, cos5, tan2, tan3, tan4, tan5, asin2, asin3, acos2, acos3, atan2, atan3}, setify(listofvars(ans1))) ) then 1 else false;
fxalart_set : intersection({x, y, s, t, fx, fy, fxx, fxy, fyx, fyy}, setify(listofops(ans1)));
fxalart_elem : if not emptyp( fxalart_set ) then listify(fxalart_set)[1];
fxalart : if not emptyp( fxalart_set ) then 1 else false;
stackqsimp(ex) := ratsimp( radcan( exponentialize(ex) ) );
does_hold(ex) := is( stackqsimp(ex) );
ans1 : ratsubst(fxy, fyx, ans1);
a1 : ZZZ;
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS


  Feedbk02 = <<EOS.chop
<![CDATA[
myargs(xs) := block([as, zzz],as : if atom(xs) then xs else args(xs),if not chk_op(as, xs) then return(zzz),as);
chk_op(as, xs) := block([op1, x],if not( atom(as) ) and not( atom(xs) ) then (if member(x, as) then (op1 : op(xs),return( member(op1, ["and", "or", "<", ">", ">=", "<="]) ))),true);
edges(xs) := block([x],delete(x, flatten( scanmap(myargs, xs))));
xs_in_interval(xs, cond) := block(map(lambda([x], charfun(cond)), xs));
is_same_interval(c1, c2) := block([ret, xs1, xs2, v1, v2, x, m],ret : true,xs1 : edges(c1),xs2 : edges(c2),m : lmax( map(abs, append(xs1, xs2)) ),m : 2*min(max(m, 1), 100),ret : ret and is(xs_in_interval(xs1, c1) = xs_in_interval(xs1, c2)),ret : ret and is(xs_in_interval(xs2, c1) = xs_in_interval(xs2, c2)),if ret then (v1 : quad_qags(charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],v2 : quad_qags(charfun(c2)*charfun(c1), x, -m, m, 'epsrel=10^(-12) )[1],ret : ret and is(v1 = v2)),ret);

a1 : abcd;
result : if is_same_interval(a1, ans1) then 1 else false;
]]>
EOS

  Kekka02 = <<EOS.chop
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
      <text>abs</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p>abs</p>
<p>\\(\\,\\)  xyz <p> [[input:ans1]] [[input:ans2]] [[input:ans3]]</p>
<div>[[validation:ans1]] [[validation:ans2]] [[validation:ans3]]</div>]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.0000000</penalty>
    <hidden>0</hidden>
    <questionvariables>
      <text>k1 : [[1,1,0], [1,0,0]];</text>
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
      <type>matrix</type>
      <tans>matrix([1],[1],[1])</tans>
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
    <input>
      <name>ans2</name>
      <type>matrix</type>
      <tans>matrix([1],[1],[1])</tans>
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
    <input>
      <name>ans3</name>
      <type>matrix</type>
      <tans>matrix([1],[1],[1])</tans>
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

    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><![CDATA[
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(radcan(a)),x0 : listify(radcan(x)),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
is_basis(x) := block([ret, x0, xm, i, n], ret : true, x0 : x, xm : apply(matrix, x0), ret: true, n : -(length(x0)+1), for i:1 thru length(x0) do (m : apply(matrix, append(rest(x0,i), rest(x0,n+i))), ret : ret and is(rank(m) + 1 = rank(xm))), ret) ;
is_orthonormal_basis(x) := block([xm], xm : apply(matrix, radcan(x)), if is( ratsimp( ident(length(x)) = xm.conjugate(transpose(xm)) ) ) then true else false) ;
b1 : delete([N, N, N], [list_matrix_entries(ans1), list_matrix_entries(ans2), list_matrix_entries(ans3)]);
result : if is_same_linear_space(k1, b1) and is_basis(b1) then true else false;
]]></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest>CasEqual</answertest>
        <sans>result</sans>
        <tans>true</tans>
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

end

# -*- coding: utf-8 -*-

class STACK_Q
  include ERB::Util
  HEAD = <<"EOS"
<?xml version="1.0" encoding="UTF-8"?>
<quiz>
<!-- question: 0  -->
  <question type="category">
    <category>
        <text>$course$/<%=h @category %></text>

    </category>
  </question>
EOS

  TMPL_with_single_input = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname_0  %></text>
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
      <forbidwords>[[BASIC-ALGEBRA]],[[BASIC-CALCULUS]],[[BASIC-MATRIX]],logx,logy,min,max<%=h forbidwords %> </forbidwords>
      <allowwords>fx, fy, fxx, fxy, fyx, fyy</allowwords>
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
        <sans>result</sans>
        <tans>1</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
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
        <falsenextnode>1</falsenextnode>
        <falseanswernote>prt1-1-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>1</name>
        <answertest>AlgEquiv</answertest>
        <sans>xyalart</sans>
        <tans>1</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-2-T</trueanswernote>
        <truefeedback format="html">
          <text><![CDATA[<p></p><p>【ヒント】x*y などを xy と*なしで入力していませんか。<br>【要確認】あなたの解答の中の {@xyalart_elem@} を確認して下さい。</p><p><br></p>]]></text>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>2</falsenextnode>
        <falseanswernote>prt1-2-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>2</name>
        <answertest>AlgEquiv</answertest>
        <sans>sinalart</sans>
        <tans>1</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-3-T</trueanswernote>
        <truefeedback format="html">
          <text><![CDATA[<p></p><p><br></p><p>【注意】sin(2*x) などを sin2*x と( )なしで入力していませんか。【要確認】</p><p><br></p><p></p>]]></text>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>3</falsenextnode>
        <falseanswernote>prt1-3-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>3</name>
        <answertest>AlgEquiv</answertest>
        <sans>fxalart</sans>
        <tans>1</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-4-T</trueanswernote>
        <truefeedback format="html">
          <text><![CDATA[<p></p><p>【ヒント】x*(x+y) や fx*(x+y) などを x(x+y) や fx(x+y) と*なしで入力していませんか。<br>【要確認】あなたの解答の中の {@fxalart_elem@}(...) を確認して下さい。</p><p><br></p>]]></text>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>4</falsenextnode>
        <falseanswernote>prt1-4-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>4</name>
        <answertest>AlgEquiv</answertest>
        <sans>1</sans>
        <tans>2</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-5-T</trueanswernote>
        <truefeedback format="html">
          <text></text>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>-1</falsenextnode>
        <falseanswernote>prt1-5-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
    </prt>
  </question>
EOS

  TMPL_with_multi_input = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname_0  %></text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p><%=h qname  %></p>
<p><%=h qstr  %> <%= ans_forms %>]]></text>
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
<%= ans_inputs %>
    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><%= feedbk %></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest>CasEqual</answertest>
        <sans>result</sans>
        <tans>true</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
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

  TMPL_with_ith = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname_0  %></text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p><%=h qname  %></p>
<p><%=h qstr  %> <%= ans_forms %>]]></text>
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
<%= ans_inputs %>
    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><%= feedbk %></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest>CasEqual</answertest>
        <sans>result</sans>
        <tans>true</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
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
        <falsenextnode>1</falsenextnode>
        <falseanswernote>prt1-1-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>1</name>
        <answertest>CasEqual</answertest>
        <sans>ith</sans>
        <tans>0</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-2-T</trueanswernote>
        <truefeedback format="html">
<% unless quiz.ans_num == 1 -%>
          <text><![CDATA[<p>【ヒント】解答中に同じ解答が複数あります。<br></p>]]></text>
<% end -%>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>-1</falsenextnode>
        <falseanswernote>prt1-2-F</falseanswernote>
        <falsefeedback format="html">
<% unless quiz.ans_num == 1 -%>
          <text><![CDATA[<p>【ヒント】@ith@番目の解答のが違います。<br></p>]]></text>
<% end -%>
        </falsefeedback>
      </node>
    </prt>
  </question>
EOS

  TMPL_eigen = <<"EOS"
  <question type="stack">
    <name>
      <text><%=h qname_0  %></text>
    </name>
    <questiontext format="html">
      <text><![CDATA[<p><%=h qname  %></p>
<p><%=h qstr  %> <%= ans_forms %>]]></text>
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
<%= ans_inputs %>
    <prt>
      <name>prt1</name>
      <value>1.0000000</value>
      <autosimplify>1</autosimplify>
      <feedbackvariables>
        <text><%= feedbk %></text>
      </feedbackvariables>
      <node>
        <name>0</name>
        <answertest>CasEqual</answertest>
        <sans>result</sans>
        <tans>true</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
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
        <falsenextnode>1</falsenextnode>
        <falseanswernote>prt1-1-F</falseanswernote>
        <falsefeedback format="html">
          <text></text>
        </falsefeedback>
      </node>
      <node>
        <name>1</name>
        <answertest>CasEqual</answertest>
        <sans>ith</sans>
        <tans>0</tans>
        <testoptions></testoptions>
        <quiet>1</quiet>
        <truescoremode>+</truescoremode>
        <truescore>0.0000000</truescore>
        <truepenalty></truepenalty>
        <truenextnode>-1</truenextnode>
        <trueanswernote>prt1-2-T</trueanswernote>
        <truefeedback format="html">
<% unless quiz.eigen_val_num == 1 -%>
          <text><![CDATA[<p>【ヒント】解答中に同じ値の固有値が複数あります。<br></p>]]></text>
<% end -%>
        </truefeedback>
        <falsescoremode>-</falsescoremode>
        <falsescore>0.0000000</falsescore>
        <falsepenalty></falsepenalty>
        <falsenextnode>-1</falsenextnode>
        <falseanswernote>prt1-2-F</falseanswernote>
        <falsefeedback format="html">
<% unless quiz.eigen_val_num == 1 -%>
          <text><![CDATA[<p>【ヒント】@ith@番目の解答の固有値か固有ベクトルが違います。<br></p>]]></text>
<% end -%>
        </falsefeedback>
      </node>
    </prt>
  </question>
EOS

  FOOT = "</quiz>"
end


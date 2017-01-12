require "stackq/base"

class STACK_Q

class Is_P_and_PAP < StackqBase

  def initialize(*args)
    super
    @dim = basis_dim(@a1)
  end

  def template
    TMPL_with_multi_input
  end

  def ans_inputs
     multi_input([ ["ans1", "matrix", [@dim,@dim]],
                   ["ans2", "matrix", [@dim,@dim]] ])
  end

  def feedbk
    <<EOS.chomp
<![CDATA[
is_diagonal(m) := block([col_size, row_size],col_size : length(m),row_size : length(m[1]),is(col_size = row_size) and is( m = m * diagmatrix(col_size, 1)));
#{does_hold_mac}
result: if is(rank(ans1) = length(ans1)) and is_diagonal(ans2) and does_hold(k1.ans1 = ans1.ans2) then true else false;
]]>
EOS
  end

  def ans_forms
    desc_varnames_forms([['\(P=\)', "ans1"], ['\(P^{-1}AP=\)', "ans2"]], nline: true)
  end
end

end

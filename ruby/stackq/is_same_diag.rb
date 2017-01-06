require "stackq/base"

class STACK_Q

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

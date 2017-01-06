require "stackq/base"

class STACK_Q

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

end

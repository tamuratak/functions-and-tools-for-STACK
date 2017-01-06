require "stackq/feedbkutil"

class STACK_Q

module BasisUtil
  include FeedbkUtil

  def basis_feedback_lib_mac
<<HERE.chomp
#{does_hold_mac}
is_same_linear_space(a, x) := block([ret, a0, x0, am, xm, am_dim, i],ret : true,a0 : listify(radcan(a)),x0 : listify(radcan(x)),am : apply(matrix, a0),xm : apply(matrix, x0),ret: ret and is(rank(am) = rank(xm)),if ret then (am_dim : rank(am),for i:1 thru length(x0) do (m : apply(matrix, cons(x0[i], a0)),ret : ret and is(rank(m) = am_dim))),ret);
is_basis(x) := block([xm],xm : apply(matrix, x),is( rank(xm) = length(x) ));
is_orthonormal_basis(x) := block([xm, tmp],xm : apply(matrix, radcan(x)),tmp : xm.(conjugate(transpose(xm))),does_hold( ident(length(x)) = tmp ) or does_hold( 1 = tmp ));
HERE
  end

  def basis_ans(n, dim, input_size, prefix="")
    ret = ""
    (1..n).each do |i|
      ret << one_input("ans"+prefix+i.to_s, "matrix", dims: [dim, 1], input_size: input_size)
    end
    ret
  end

  def basis_chk(mthd)
    case mthd
    when /orthonormal/
      "is_orthonormal_basis"
    else
      "is_basis"
    end
  end

end

end

class STACK_Q

module FeedbkUtil
  module_function

  def does_hold_mac
    <<EOS.chomp
stackqsimp(ex) := fullratsimp( radcan( factcomb( exponentialize(ex) ) ) );
does_hold(ex) := is( stackqsimp(lhs(ex)-rhs(ex)=0) or ratsimp(ex) );
declare(n, integer);
EOS
  end

  def feedbk_alart
    fun_num_list = ["sin", "cos", "tan", "asin", "acos", "atan", "exp", "log"].product((0..9).to_a).map(&:join).join(", ")
    <<EOS.chomp
listofops(x) := block([], if not atom(x) then cons( op(x), flatten(map(listofops, args(x))) ) else [] );
xyalart_set : intersection({xy, yx}, setify( append(listofvars(ans1), listofops(ans1))   ));
xyalart_elem : if not emptyp( xyalart_set ) then listify(xyalart_set)[1];
xyalart : if not emptyp( xyalart_set ) then 1 else false;
sinalart : if not emptyp( intersection({#{fun_num_list}}, setify(listofvars(ans1))) ) then 1 else false;
fxalart_set : intersection({x, y, s, t, fx, fy, fxx, fxy, fyx, fyy}, setify(listofops(ans1)));
fxalart_elem : if not emptyp( fxalart_set ) then listify(fxalart_set)[1];
fxalart : if not emptyp( fxalart_set ) then 1 else false;
#{does_hold_mac}
ans1 : ratsubst(fxy, fyx, ans1);
EOS
  end

end

end

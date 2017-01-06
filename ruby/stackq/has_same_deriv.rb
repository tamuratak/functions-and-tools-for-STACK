require "stackq/base"

class STACK_Q

class Has_same_deriv < StackqBase

  def feedbk
      <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
a1 : diff(a1,x);
ans1 : diff(ans1, x);
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
  end

  def stack_mthd
    "AlgEquiv"
  end
end

end

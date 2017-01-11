require "stackq/base"

class STACK_Q

class AlgEquiv < Stackq_with_single_input
  def initialize(*args)
    super
  end

  def t_ans1
    cdata(@a1)
  end

  def feedbk
    <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
result : if does_hold( a1 = ans1 ) then 1 else false;
]]>
EOS
  end

  def stack_mthd
    if @mthd == "CasEqualNotAsin"
      "CasEqual"
    else
      @mthd
    end
  end

  def forbidwords
    if @mthd == "CasEqualNotAsin"
      ",asin,acos,atan"
    else
      ""
    end
  end
end

end

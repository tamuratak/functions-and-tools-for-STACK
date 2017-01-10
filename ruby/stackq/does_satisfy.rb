require "stackq/base"

class STACK_Q

class Does_satisfy < StackqBase

  def feedbk
      <<EOS.chomp
<![CDATA[
#{feedbk_alart}
a1 : #{esq_cdata(@a1)};
result : if #{esq_cdata(does_satisfy_ex(@ext))} then 1 else false;
]]>
EOS
  end

end

end

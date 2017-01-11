require "stackq/base"

class STACK_Q

class Does_satisfy < Stackq_with_single_input

  def does_satisfy_ex(ext)
    if /\A\(.*?\)\s*((and|or)\s*\(.*?\))*\z/ =~ ext
      ext.gsub(/\((.*?)\)\s*(and|or|\z)/){|s|
        e1 = $1
        e2 = $2
        if /\Anot (.*)/ =~ e1
          "not does_hold(" + $1 + ") " + e2
        else
          "does_hold(" + e1 + ") " + e2
        end
      }
    else
      raise "format invalid for does_satisfy"
    end
  end

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

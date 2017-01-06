require "stackq/util"
require "stackq/feedbkutil"

class STACK_Q

class StackqBase
  include StackqUtil
  include FeedbkUtil

  def initialize(a1=nil, input_size: 15, line_num: nil, mthd: nil, ext: nil)
    @a1 = a1
    @input_size = input_size
    @line_num = line_num
    @mthd = mthd
    @ext = ext
  end
  attr_accessor :mthd

  def t_ans1
    cdata(@a1)
  end

  def stack_mthd
    "CasEqual"
  end

  def forbidwords
    ""
  end

  def input_type
    if is_matrix_type(@a1)
      "matrix"
    else
      "algebraic"
    end
  end

  def input_size
    if is_matrix_type(@a1)
      15
    else
      100
    end
  end
end

end

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
  attr_reader :mthd

  def forbidwords
    ""
  end

end

end

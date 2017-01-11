require "stackq/base"

class STACK_Q

class Stackq_with_single_input < StackqBase

  def t_ans1
    cdata(@a1)
  end

  def stack_mthd
    "CasEqual"
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

  def template
    TMPL_with_single_input
  end

end

end

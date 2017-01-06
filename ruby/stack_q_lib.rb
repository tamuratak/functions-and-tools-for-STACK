# -*- coding: utf-8 -*-
require 'erb'
require 'stack_q_tmpl'
require "stackq/base"
require "stackq/is_P_and_PAP"
require "stackq/eigen_multiplicity_eq"
require "stackq/basisutil"
require "stackq/is_basis_of_same_linear_space"
require "stackq/is_same_eigenval_and_eigenvec"
require "stackq/is_same_linear_eq"
require "stackq/algEquiv.rb"
require "stackq/has_same_deriv"
require "stackq/does_satisfy"
require "stackq/is_same_diag"

class STACK_Q
include CDATAUtil

  def initialize(s, opt = {})
    @txt = s
    @err_msg = ""
    @category = opt["category"] || "stack_q"
    @opt = opt
  end
  attr_reader :err_msg

  def txt2xml
    ret = ""
    ret << ERB.new(HEAD).result(binding)
    line_num = 1
    @sort_prefix0 ||= sort_prefix()

    @txt.each_line{|l|
      next if /\A\s*\Z/ =~ l
      @err_msg = "error at line: #{line_num}"

      qname, qstr, a1, mthd, ext = l.split(/\s*\*\*\s*/).map{|s| s.sub(/\A\s*/, "").sub(/\s*\Z/, "") }
      mthd = mthd || "AlgEquiv"
      validate_maxima_exp(a1, line_num, l)

      # workaround for Moodle + MathJax bug
      qstr = '\(\,\)  ' + inline_tex(qstr)

      # teacher's answer == a1 == t_ans1, (prt stands for potential response tree)
      # student's answer == ans1
      unless ret0 = ( txt2xml_with_single_input(qname, qstr, a1, mthd, ext, line_num) or
                      txt2xml_with_multi_input(qname, qstr, a1, mthd, ext, line_num) )
        raise "invalid grading method"
      end

      ret << ret0
      line_num  += 1
    }
    
    ret << FOOT
  end

  def qname_0(qname, line_num)
    if @opt["sort-prefix"]
      ln = "%.2d" % line_num
      @sort_prefix0 + "-" + ln + "-" + qname
    else
      qname
    end
  end

  def txt2xml_with_single_input(qname, qstr, a1, mthd, ext, line_num)
    qname_0 = qname_0(qname, line_num)
    input_size = @opt["form-size"] || 15

    case mthd
    when "AlgEquiv", "CasEqualNotAsin"
      klass = AlgEquiv
    when "is_same_linear_eq", "has_same_nullspace", "is_same_plane"
      klass = Is_same_linear_eq
    when "has_same_deriv"
      klass = Has_same_deriv
    when "does_satisfy"
      klass = Does_satisfy
    when "is_same_diag"
      klass = Is_same_diag
    else
      return nil
    end

    x = ERB.new(TMPL)
    quiz = klass.new(a1, input_size: input_size, mthd: mthd, ext: ext)
    t_ans1 = quiz.t_ans1
    feedbk = quiz.feedbk
    stack_mthd = quiz.stack_mthd
    forbidwords = quiz.forbidwords
    input_type = quiz.input_type
    input_size = quiz.input_size

    x.result(binding)
  end

  # we have to set (inputs + feedback + answer forms)
  # inputs == student answer type + form size + etc
  def txt2xml_with_multi_input(qname, qstr, a1, mthd, ext, line_num)
    qname_0 = qname_0(qname, line_num)
    input_size = @opt["form-size"] || 15

    case mthd
    when "eigen_multiplicity_eq"
      klass = Eigen_multiplicity_eq
      tmpl = TMPL_with_ith
    when "is_P_and_PAP"
      klass = Is_P_and_PAP
      tmpl = TMPL_with_multi_input
    when "is_basis_of_same_linear_space", "is_orthonormal_basis_of_same_linear_space"
      klass = Is_basis_of_same_linear_space
      tmpl = TMPL_with_multi_input
    when "is_same_eigenval_and_eigenvec", "is_same_eigenval_and_orthonormal_eigenvec"
      klass = Is_same_eigenval_and_eigenvec
      tmpl = TMPL_eigen
    else
      return nil
    end

    x = ERB.new(tmpl, nil, '-')
    quiz = klass.new(a1, input_size: input_size, mthd: mthd, ext: ext)
    ans_inputs = quiz.ans_inputs
    feedbk = quiz.feedbk
    ans_forms = quiz.ans_forms

    x.result(binding)
  end

  def inline_tex(s)
    s.gsub(/([^\\]|\A)\$((\\\$|[^\$])*)\$/) { $1 + '\\(' + $2 + '\\)' }
  end
  
  def validate_maxima_exp(s, line_num = 1, l = "")
    tmp = s
    until tmp == " XXX "
      prev = tmp
      tmp = tmp.gsub(/(?<=\A|[\(\[\{,]|and|or|not)\s*-?(\s*([a-zA-Z]\w*|\d+|%e|%pi|%i)\s*([\*\+\-\^\/\=]|[\>\<]=?))*\s*([a-zA-Z]\w*\!?|\d+\!?|%e|%pi|%i)\s*(?=\z|[\)\]\},]|and|or)/, " XXX ")
      5.times{
        tmp = tmp.gsub(/(?!(and|or|not)\s*\()([a-z]{3,})\s*\(( XXX ,)* XXX \)/, " XXX ")
      }
      tmp = tmp.gsub(/\( XXX \)/, " XXX ")
      tmp = tmp.gsub(/\[( XXX ,)* XXX \]/, " XXX ")
      tmp = tmp.gsub(/\{( XXX ,)* XXX \}/, " XXX ")
      tmp = tmp.gsub(/ XXX (and|or) XXX /, " XXX ")
      tmp = tmp.gsub(/not XXX/, " XXX ")
      if tmp == prev
        @err_msg = "validation error of maxima expression at line #{line_num}" + "\n"
        raise "\n\n" + @err_msg + l + "\n\n"
      end
    end

    return true
  end

  def sort_prefix
    today = Time.now
    if [1, 2, 3].include?( today.month )
      num = Time.new(today.year, 4, 1) - today
    else
      num =  Time.new(today.year+1, 4, 1) - today
    end
    num = num.round / (60*60)
    "%.4d" % num
  end
 
  def plane_type_check(s, line_num)
    unless /\A\[[^\[\]]*?\]\z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
  end

  def eq_type_check(s, line_num)
    unless /\A\{[^\{\}]*?\}\z/ =~ s
      @err_msg = "error at line: #{line_num}" + "\n" + "invalid answer type"
      raise "invalid answer type"
    end
  end

end

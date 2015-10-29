# -*- coding: utf-8 -*-
require 'test/unit'

$LOAD_PATH.push File.dirname(File.dirname(File.expand_path(__FILE__)))
require 'math2maxi'

class TestMath2Maxima < Test::Unit::TestCase

  def test_to_maxima
    assert_equal("[[1, 2, 3]]",
                 Math2Maxima.to_maxima("{{1, 2, 3}}"))
    assert_equal("[[1, 2, 3], [1, 1, 1]]",
                 Math2Maxima.to_maxima("{{1, 2, 3}, {1, 1, 1}}"))
    assert_equal("a*b",
                 Math2Maxima.to_maxima("a b"))
  end

end

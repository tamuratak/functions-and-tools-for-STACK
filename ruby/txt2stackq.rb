#!/usr/bin/ruby
# -*- coding: utf-8 -*-

f = File.expand_path(__FILE__)
if File.symlink?(f)
  f = File.readlink(f)
end

$LOAD_PATH.push File.dirname(f)

require 'stack_q_lib'
require 'optparse'

opt = ARGV.getopts("", "category:", "sort-prefix", "form-size:")
puts STACK_Q.new(ARGF.read, opt).txt2xml

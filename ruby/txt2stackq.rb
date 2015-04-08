#!/usr/bin/ruby
# -*- coding: utf-8 -*-

$LOAD_PATH.push File.dirname(File.expand_path(__FILE__))

require 'stack_q_lib'

puts STACK_Q.new(ARGF.read).txt2xml

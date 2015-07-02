#!/usr/bin/ruby

require 'optparse'

caret = true
opt = ARGV.getopts("Cm")

loop do
  l = STDIN.gets
  l = l.gsub(/\\/, "")
#  l = l.gsub(/(\W)Pi(\W)/){|s| $1 + "%pi" + $2}
  l = l.gsub(/(\w+)/){|s| $1.downcase }
  5.times{
    l = l.gsub(/\[([^\[\]]+)\]/){|s| "(" + $1 + ")" }
  }
  l = l.gsub(/([\d\)])\s*([a-z\(])/){|s| $1 + "*" + $2}
  l = l.gsub(/([a-z]+)\s+([a-z]+\()/){|s| $1 + "*" + $2}
  l = l.gsub(/\(pi\)/){ "Pi" }
  [["arctan", "atan"],
   ["arcsin", "asin"],
   ["arccos", "acos"]].each{|a, b|
    l = l.gsub(/#{a}/){b}
  }
  l = l.gsub(/\^/, "\\caret") if opt["C"]

  if opt["m"]
    if /\A\{([^\{\}]+)\}/ =~ l
      p $1
      l = "matrix(" + $1.split(",").map{|e| "[" + e + "]"}.join(", ") + ")"
    elsif /\A\{(.*)\}/ =~ l
      l = "matrix(" + $1 + ")"
      l = l.gsub(/\{/, "[").gsub(/\}/, "]")
    end
  else
    l = l.gsub(/\{/, "[").gsub(/\}/, "]")
  end
  puts
  puts l
  puts 
  puts
end

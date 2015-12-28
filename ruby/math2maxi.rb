#!/usr/bin/ruby

require 'optparse'
require 'open3'

caret = true
opt = ARGV.getopts("Cm")

class Math2Maxima
  def self.to_maxima(l, opt = {})

    l = l.gsub('\[PartialD]f/\[PartialD]x', "fx")
    l = l.gsub('\[PartialD]f/\[PartialD]y', "fy")
    l = l.gsub('\[PartialD]^2f/(\[PartialD]x\[ThinSpace]\[PartialD]y)', "fxy")
    l = l.gsub('\[PartialD]^2f/(\[PartialD]y\[ThinSpace]\[PartialD]x)', "fxy")
    l = l.gsub('\[PartialD]^2f/\[PartialD]x^2', "fxx")
    l = l.gsub('\[PartialD]^2f/\[PartialD]y^2', "fyy")

    l = l.gsub(/cos\^(.*?)\((.*?)\)/){ "cos(" + $2 + ")^(" + $1 + ")" }
    l = l.gsub(/sin\^(.*?)\((.*?)\)/){ "sin(" + $2 + ")^(" + $1 + ")" }

    l = l.gsub(/\\/, "")
    l = l.gsub(/(?<!\w)I(?!\w)/){|s| "%i" }
    l = l.gsub(/(?<!\w)E(?!\w)/){|s| "%e" }
    l = l.gsub(/(\w+)/){|s| $1.downcase }
    5.times{
      l = l.gsub(/\[([^\[\]]+)\]/){|s| "(" + $1 + ")" }
      l = l.gsub(/([\d\)])\s*([a-z\(%])/){|s| $1 + "*" + $2}
      l = l.gsub(/([a-z]+)\s+([a-z%\(]+)/){|s| $1 + "*" + $2}
    }
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
    l
  end
end

if __FILE__ == $0
  loop do
    l = STDIN.gets.chomp
    l = Math2Maxima.to_maxima(l, opt)
    Open3.capture2("/usr/bin/pbcopy", :stdin_data => l)
    puts
    puts "copying: #{l}"
    puts
    puts
  end
end

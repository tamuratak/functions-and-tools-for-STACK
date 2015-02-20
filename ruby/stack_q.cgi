#!/usr/local/bin/ruby
# -*- coding: utf-8 -*-

require 'cgi'
require 'erb'

$LOAD_PATH.push File.dirname(File.expand_path(__FILE__))
require 'stack_q_lib'

cgi = CGI.new

if ("POST" == cgi.request_method and cgi['probs']) or not cgi.request_method
  begin
    values = cgi['probs']
    ret = STACK_Q.txt2xml(values)

    fname =  "stack_q_" + Time.now.strftime("%Y%m%d%H%M") + ".xml"
    puts "Content-type: application/xml; charset=UTF-8"
    puts "Content-Disposition: attachment; filename=\"#{fname}\""
    puts
    puts ret
  rescue => evar
    puts "Content-Type: text/html"
    puts
    puts ERB::Util.h(evar)
  end
else
  puts "Content-Type: text/html"
  puts 
  puts "<a href='stack_q.html'>link</a>"
  exit
end

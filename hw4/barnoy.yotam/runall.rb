#!/usr/bin/ruby

require 'optparse'
require 'pathname'

files = ["house-votes-84", "monks-1", "monks-2", "monks-3", "mushroom", "splice"]
print "#{ARGV}\n"
type = ARGV.take 1
args = ARGV.drop 1

args_str = ""
for s in args
    args_str = args_str + " " + s
end
    

files.each {|f|
    path = File.join("./data/","#{f}.data")
    outfile = File.join("./output/", "#{f}.#{type}")
    str = "./run#{type}.sh #{path} #{args_str} > #{outfile} 2>&1"
    print "#{str}\n"
    `#{str}`
}


#!/usr/bin/ruby

require 'optparse'
require 'pathname'

files = ["house-votes-84.data", "monks-1", "monks-2", "monks-3", "mushroom", "splice"]
a = ARGV.drop 1
type = a.take 1
args = a.drop 1

files.each {|f|
    path = File.join("./data/","#{f}.data")
    outfile = File.join("./output/", "#{f}.#{type}")
    str = "run#{type}.sh #{path} #{args} > outfile 2>&1"
    print str
    `#{str}`
}


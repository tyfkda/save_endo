#!ruby
require 'optparse'

UpperA = 'A'.ord
UpperZ = 'Z'.ord
LowerA = 'a'.ord

Shift = 13

def caesar(text, n)
  text.gsub(/[A-Za-z]/) do |c|
    chr = c.ord
    base = chr <= UpperZ ? UpperA : LowerA
    (((c.ord - base) + n) % 26 + base).chr
  end
end

if $0 == __FILE__
  shift = Shift
  opt = OptionParser.new
  opt.on('-s val') {|v| shift = v.to_i}
  opt.parse!(ARGV)

  while gets
    print caesar($_, shift)
  end
end

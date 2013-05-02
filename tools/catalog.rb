#!ruby

# Convert catalog page number to DNA prefix.
#
# Usage:
#  $ ruby catalog.rb 1337
#  IIPIFFCPICFPPICIICCCCCCCCCCCCIICIPPPFCCFFFCCFCFIIC

$:.unshift(File.dirname(File.expand_path(__FILE__)))
require 'util'

if $0 == __FILE__
  page_no = ARGV.shift.to_i
  page_code = asnat(page_no)
  src = "C" * page_code.length
  puts "IIPIFFCPICFPPICIIC#{src}IICIPPP#{protect(1, page_code)}IIC"
end

#!ruby

# Convert catalog page number to DNA prefix.
#
# Usage:
#  $ ruby catalog.rb 1337
#  IIPIFFCPICFPPICIICCCCCCCCCCCCIICIPPPFCCFFFCCFCFIIC

Encode = {'0' => 'C', '1' => 'F'}

page_no = ARGV.shift.to_i
page_code = sprintf("%b", page_no).reverse.gsub(/[01]/, Encode)
src = "C" * page_code.length
puts "IIPIFFCPICFPPICIIC#{src}IICIPPP#{page_code}IIC"

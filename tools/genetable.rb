#!ruby

# Show Gene Table.
#
# Usage:
#  $ ruby genetable.rb <page number>
#

$:.unshift(File.dirname(File.expand_path(__FILE__)))
require 'util'

GeneTablePageNum = 42

def intcode(x)
  protect(1, asnat(x))
end

def skip_length(n)
  "IP#{asnat(intcode(n).length)}P"
end

def search(dna)
  "IFF#{protect(1, dna)}"
end

def ref(n, l)
  "IP#{asnat(l)}P#{asnat(n)}P"
end

if $0 == __FILE__
  page_no = ARGV.shift.to_i
  puts (# Pattern
        "IIP#{search("IFPCFFP")}IIC#{skip_length(GeneTablePageNum)}" +
        "IIP#{search("IFPFIP")}IIC#{skip_length(page_no)}IIC" +
        # Template
        "#{ref(0,0)}#{intcode(GeneTablePageNum)}" +
        "#{ref(1,0)}#{intcode(page_no)}IIC")
end

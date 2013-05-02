#!ruby

# Encode consts.

QuoteEncode = {'I' => 'C', 'C' => 'F', 'F' => 'P', 'P' => 'IC'}

def quote(dna)
  dna.each_char.to_a.map{|c| QuoteEncode[c]}.join
end

def protect(l, dna)
  l.times do
    dna = quote(dna)
  end
  return dna
end

BinEncode = {'0' => 'I', '1' => 'C'}

def asnat(num)
  sprintf("%b", num).reverse.gsub(/[01]/, BinEncode)
end

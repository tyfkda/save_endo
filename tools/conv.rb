def main
  all = $stdin.read
  puts "functable = {"
  all.scan(/^([0-9a-f]{6}) ([0-9a-f]{6}) ([^ \n]+)$/m) do |m|
    print <<EOD
  "#{$3}" => { :offset => 0x#{$1}, :size => 0x#{$2} },
EOD
  end
  puts "}"
end

main

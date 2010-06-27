# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

eval q< '-' =~ /(あ[い-あ])/ >;
if ($@) {
    print "ok - 1 $^X 149_jperlre.t die ('-' =~ /あ[い-あ]/).\n";
}
else {
    print "not ok - 1 $^X 149_jperlre.t die ('-' =~ /あ[い-あ]/).\n";
}

__END__

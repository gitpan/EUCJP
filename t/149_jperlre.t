# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

eval q< '-' =~ /(��[��-��])/ >;
if ($@) {
    print "ok - 1 $^X 149_jperlre.t die ('-' =~ /��[��-��]/).\n";
}
else {
    print "not ok - 1 $^X 149_jperlre.t die ('-' =~ /��[��-��]/).\n";
}

__END__

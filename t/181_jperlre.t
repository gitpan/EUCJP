# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

local $^W = 0;
if (1) {
    print "ok - 1 # SKIP $^X $__FILE__ not ('��' =~ /$��/).\n";
    exit;
}

if ('��' =~ /($��)/) {
    print "not ok - 1 $^X $__FILE__ not ('��' =~ /$��/).\n";
}
else {
    print "ok - 1 $^X $__FILE__ not ('��' =~ /$��/).\n";
}

__END__

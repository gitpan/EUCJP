# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('����q' =~ /(����{1,}����)/) {
    print "not ok - 1 $^X $__FILE__ not ('����q' =~ /����{1,}����/).\n";
}
else {
    print "ok - 1 $^X $__FILE__ not ('����q' =~ /����{1,}����/).\n";
}

__END__

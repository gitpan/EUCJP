# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..6\n";

my $__FILE__ = __FILE__;

if ('��ABC DEF GHI' =~ /\bABC/) {
    print "ok - 1 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bABC/)\n";
}
else {
    print "not ok - 1 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bABC/)\n";
}

if ('��ABC DEF GHI' =~ /\bDEF/) {
    print "ok - 2 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bDEF/)\n";
}
else {
    print "not ok - 2 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bDEF/)\n";
}

if ('��ABC DEF GHI' =~ /\bGHI/) {
    print "ok - 3 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bGHI/)\n";
}
else {
    print "not ok - 3 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\bGHI/)\n";
}

if ('��ABC DEF GHI' =~ /ABC\b/) {
    print "ok - 4 $^X $__FILE__ ('��ABC DEF GHI' =~ /ABC\\b/)\n";
}
else {
    print "not ok - 4 $^X $__FILE__ ('��ABC DEF GHI' =~ /ABC\\b/)\n";
}

if ('��ABC DEF GHI' =~ /DEF\b/) {
    print "ok - 5 $^X $__FILE__ ('��ABC DEF GHI' =~ /DEF\\b/)\n";
}
else {
    print "not ok - 5 $^X $__FILE__ ('��ABC DEF GHI' =~ /DEF\\b/)\n";
}

if ('��ABC DEF GHI' =~ /GHI\b/) {
    print "ok - 6 $^X $__FILE__ ('��ABC DEF GHI' =~ /GHI\\b/)\n";
}
else {
    print "not ok - 6 $^X $__FILE__ ('��ABC DEF GHI' =~ /GHI\\b/)\n";
}

__END__

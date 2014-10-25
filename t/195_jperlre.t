# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..7\n";

my $__FILE__ = __FILE__;

if ('ABC DEF GHI' =~ /\BABC/) {
    print "not ok - 1 $^X $__FILE__ ('ABC DEF GHI' =~ /\\BABC/)\n";
}
else {
    print "ok - 1 $^X $__FILE__ ('ABC DEF GHI' =~ /\\BABC/)\n";
}

if ('��ABC DEF GHI' =~ /\BABC/) {
    print "ok - 2 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BABC/)\n";
}
else {
    print "not ok - 2 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BABC/)\n";
}

if ('��ABC DEF GHI' =~ /\BDEF/) {
    print "not ok - 3 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BDEF/)\n";
}
else {
    print "ok - 3 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BDEF/)\n";
}

if ('��ABC DEF GHI' =~ /\BGHI/) {
    print "not ok - 4 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BGHI/)\n";
}
else {
    print "ok - 4 $^X $__FILE__ ('��ABC DEF GHI' =~ /\\BGHI/)\n";
}

if ('��ABC DEF GHI' =~ /ABC\B/) {
    print "not ok - 5 $^X $__FILE__ ('��ABC DEF GHI' =~ /ABC\\B/)\n";
}
else {
    print "ok - 5 $^X $__FILE__ ('��ABC DEF GHI' =~ /ABC\\B/)\n";
}

if ('��ABC DEF GHI' =~ /DEF\B/) {
    print "not ok - 6 $^X $__FILE__ ('��ABC DEF GHI' =~ /DEF\\B/)\n";
}
else {
    print "ok - 6 $^X $__FILE__ ('��ABC DEF GHI' =~ /DEF\\B/)\n";
}

if ('��ABC DEF GHI' =~ /GHI\B/) {
    print "not ok - 7 $^X $__FILE__ ('��ABC DEF GHI' =~ /GHI\\B/)\n";
}
else {
    print "ok - 7 $^X $__FILE__ ('��ABC DEF GHI' =~ /GHI\\B/)\n";
}

__END__

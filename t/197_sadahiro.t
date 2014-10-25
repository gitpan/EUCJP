# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..10\n";

my $__FILE__ = __FILE__;

# ñ�춭���򰷤��᥿ʸ�� C<\b> ����� C<\B> ��������ư��ޤ���

if ('ABC ��DEF GHI' =~ /\bDEF/) {
    print "ok - 1 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bDEF/)\n";
}
else {
    print "not ok - 1 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bDEF/)\n";
}

if ('ABC ��DEF GHI' =~ /DEF\b/) {
    print "ok - 2 $^X $__FILE__ ('ABC ��DEF GHI' =~ /DEF\\b/)\n";
}
else {
    print "not ok - 2 $^X $__FILE__ ('ABC ��DEF GHI' =~ /DEF\\b/)\n";
}

if ('ABC ��DEF GHI' =~ /\bDEF\b/) {
    print "ok - 3 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bDEF\\b/)\n";
}
else {
    print "not ok - 3 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bDEF\\b/)\n";
}

if ('ABC ��DEF GHI' =~ /\bABC/) {
    print "ok - 4 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bABC/)\n";
}
else {
    print "not ok - 4 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\bABC/)\n";
}

if ('ABC ��DEF GHI' =~ /GHI\b/) {
    print "ok - 5 $^X $__FILE__ ('ABC ��DEF GHI' =~ /GHI\\b/)\n";
}
else {
    print "not ok - 5 $^X $__FILE__ ('ABC ��DEF GHI' =~ /GHI\\b/)\n";
}

if ('ABC ��DEF GHI' =~ /\B��DEF G/) {
    print "ok - 6 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\B��DEF G/)\n";
}
else {
    print "not ok - 6 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\B��DEF G/)\n";
}

if ('ABC ��DEF GHI' =~ /��DEF G\B/) {
    print "ok - 7 $^X $__FILE__ ('ABC ��DEF GHI' =~ /��DEF G\\B/)\n";
}
else {
    print "not ok - 7 $^X $__FILE__ ('ABC ��DEF GHI' =~ /��DEF G\\B/)\n";
}

if ('ABC ��DEF GHI' =~ /\B��DEF G\B/) {
    print "ok - 8 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\B��DEF G\\B/)\n";
}
else {
    print "not ok - 8 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\B��DEF G\\B/)\n";
}

if ('ABC ��DEF GHI' =~ /\BABC/) {
    print "not ok - 9 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\BABC/)\n";
}
else {
    print "ok - 9 $^X $__FILE__ ('ABC ��DEF GHI' =~ /\\BABC/)\n";
}

if ('ABC ��DEF GHI' =~ /GHI\B/) {
    print "not ok - 10 $^X $__FILE__ ('ABC ��DEF GHI' =~ /GHI\\B/)\n";
}
else {
    print "ok - 10 $^X $__FILE__ ('ABC ��DEF GHI' =~ /GHI\\B/)\n";
}

__END__

http://search.cpan.org/dist/EUC-JP-Regexp/

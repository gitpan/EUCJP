# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..4\n";

my $__FILE__ = __FILE__;

# �᥿ʸ�� C<\U>, C<\L>, C<\Q>, C<\E> ������ѿ�Ÿ���Ϲ�θ����Ƥ���ޤ���
# ɬ�פʤ顢C<""> (or C<qq//>) �黻�Ҥ�ȤäƤ���������

if ('ABC' =~ /\Uabc\E/) {
    print "ok - 1 $^X $__FILE__ ('ABC' =~ /\\Uabc\\E/)\n";
}
else {
    print "not ok - 1 $^X $__FILE__ ('ABC' =~ /\\Uabc\\E/)\n";
}

if ('def' =~ /\LDEF\E/) {
    print "ok - 2 $^X $__FILE__ ('def' =~ /\\LDEF\\E/)\n";
}
else {
    print "not ok - 2 $^X $__FILE__ ('def' =~ /\\LDEF\\E/)\n";
}

if ('({[' =~ /\Q({[\E/) {
    print "ok - 3 $^X $__FILE__ ('({[' =~ /\\Q({[\\E/)\n";
}
else {
    print "not ok - 3 $^X $__FILE__ ('({[' =~ /\\Q({[\\E/)\n";
}

my $var = 'GHI';
if ('GHI' =~ /GHI/) {
    print "ok - 4 $^X $__FILE__ ('GHI' =~ /GHI/)\n";
}
else {
    print "not ok - 4 $^X $__FILE__ ('GHI' =~ /GHI/)\n";
}

__END__

# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..4\n";

my $__FILE__ = $__FILE__;

#
# chop, chomp
#

$y = "������";
$x = chomp($y);
if ($x ne 0 || $y ne "������") {
    print "not ok - 1 $^X $__FILE__\n";
}
else {
    print "ok - 1 $^X $__FILE__\n";
}

$y = "������\n";
$x = chomp($y);
if ($x ne 1 || $y ne "������") {
    print "not ok - 2 $^X $__FILE__\n";
}
else {
    print "ok - 2 $^X $__FILE__\n";
}

$y = "������";
$x = chop($y);
if ($x ne "��" || $y ne "����") {
    print "not ok - 3 $^X $__FILE__\n";
}
else {
    print "ok - 3 $^X $__FILE__\n";
}

$y = "������t";
$x = chop($y);
if ($x ne "t" || $y ne "������") {
    print "not ok - 4 $^X $__FILE__\n";
}
else {
    print "ok - 4 $^X $__FILE__\n";
}

__END__
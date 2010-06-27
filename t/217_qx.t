# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

my $__FILE__ = __FILE__;

use EUCJP;
print "1..1\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $0\n";
    exit;
}

open(FILE,'>F機能') || die "Can't open file: F機能\n";
print FILE "1\n";
close(FILE);

# qx
if (qx/type F機能/) {
    print "ok - 1 qx $^X $__FILE__\n";
}
else {
    print "not ok - 1 qx $^X $__FILE__\n";
}

unlink('F機能');

__END__

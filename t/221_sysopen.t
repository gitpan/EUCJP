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

# sysopen
use Fcntl;
if (sysopen(FILE,'F機能',O_RDONLY)) {
    print "ok - 1 sysopen $^X $__FILE__\n";
    close(FILE);
}
else {
    print "not ok - 1 sysopen: $! $^X $__FILE__\n";
}

unlink('F機能');

__END__

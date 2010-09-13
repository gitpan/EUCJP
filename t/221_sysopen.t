# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

my $__FILE__ = __FILE__;

use EUCJP;
print "1..1\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $0\n";
    exit;
}

open(FILE,'>F��ǽ') || die "Can't open file: F��ǽ\n";
print FILE "1\n";
close(FILE);

# sysopen
use Fcntl;
if (sysopen(FILE,'F��ǽ',O_RDONLY)) {
    print "ok - 1 sysopen $^X $__FILE__\n";
    close(FILE);
}
else {
    print "not ok - 1 sysopen: $! $^X $__FILE__\n";
}

unlink('F��ǽ');

__END__
# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

my $__FILE__ = __FILE__;

use EUCJP;
print "1..3\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $0\n";
    print "ok - 2 # SKIP $^X $0\n";
    print "ok - 3 # SKIP $^X $0\n";
    exit;
}

open(FILE,'>F��ǽ') || die "Can't open file: F��ǽ\n";
print FILE "1\n";
close(FILE);

unlink('file');

# rename (1/3)
if (rename('F��ǽ','file')) {
    print "ok - 1 rename (1/3) $^X $__FILE__\n";
}
else {
    print "not ok - 1 rename: $! $^X $__FILE__\n";
}

# rename (2/3)
if (rename('file','F��ǽ')) {
    print "ok - 2 rename (2/3) $^X $__FILE__\n";
}
else {
    print "not ok - 2 rename: $! $^X $__FILE__\n";
}

# rename (3/3)
if (rename('F��ǽ','F2��ǽ')) {
    print "ok - 3 rename (3/3) $^X $__FILE__\n";
    system('del F2��ǽ 2>NUL');
}
else {
    print "not ok - 3 rename: $! $^X $__FILE__\n";
}

unlink('F��ǽ');

__END__

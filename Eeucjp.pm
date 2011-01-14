package Eeucjp;
######################################################################
#
# Eeucjp - Run-time routines for EUCJP.pm
#
#                  http://search.cpan.org/dist/EUCJP/
#
# Copyright (c) 2008, 2009, 2010, 2011 INABA Hitoshi <ina@cpan.org>
#
######################################################################

use 5.00503;
use strict qw(subs vars);

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 0.70 $ =~ m/(\d+)/xmsg;

BEGIN {
    my $PERL5LIB = __FILE__;

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $PERL5LIB =~ s{[^/]*$}{EUCJP};
    }

    # UNIX-like system
    else {
        $PERL5LIB =~ s{[^/]*$}{EUCJP};
    }

    my @inc = ();
    my %inc = ();
    for my $path ($PERL5LIB, @INC) {
        if (not exists $inc{$path}) {
            push @inc, $path;
            $inc{$path} = 1;
        }
    }
    @INC = @inc;
}

BEGIN {

    # instead of utf8.pm
    eval q{
        no warnings qw(redefine);
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    };
    if ($@) {
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    }

    # 7.6. Writing a Subroutine That Takes Filehandles as Built-ins Do
    # in Chapter 7. File Access
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

    sub Eeucjp::binmode(*;$);
    sub Eeucjp::open(*;$@);

    if ($] < 5.006) {

        # 12.13. Overriding a Built-in Function in All Packages
        # in Chapter 12. Packages, Libraries, and Modules
        # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

        # avoid warning: Name "CORE::GLOBAL::binmode" used only once: possible typo at ...
        *CORE::GLOBAL::binmode =
        *CORE::GLOBAL::binmode = \&Eeucjp::binmode;
        *CORE::GLOBAL::open    =
        *CORE::GLOBAL::open    = \&Eeucjp::open;
    }
}

# poor Symbol.pm - substitute of real Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;

    sub gensym () {
        my $name = "GEN" . $genseq++;
        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }

    sub qualify ($;$) {
        my ($name) = @_;
        if (!ref($name) && (Eeucjp::index($name, '::') == -1) && (Eeucjp::index($name, "'") == -1)) {
            my $pkg;
            my %global = map {$_ => 1} qw(ARGV ARGVOUT ENV INC SIG STDERR STDIN STDOUT);

            # Global names: special character, "^xyz", or other.
            if ($name =~ /^(([^a-z])|(\^[a-z_]+))\z/i || $global{$name}) {
                # RGS 2001-11-05 : translate leading ^X to control-char
                $name =~ s/^\^([a-z_])/'qq(\c'.$1.')'/eei;
                $pkg = "main";
            }
            else {
                $pkg = (@_ > 1) ? $_[1] : caller;
            }
            $name = $pkg . "::" . $name;
        }
        $name;
    }

    sub qualify_to_ref ($;$) {
        no strict qw(refs);
        return \*{ qualify $_[0], @_ > 1 ? $_[1] : caller };
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

unless (eval q{ use Fcntl qw(:flock); 1 }) {
    eval q{
        sub LOCK_SH {1}
        sub LOCK_EX {2}
        sub LOCK_UN {8}
        sub LOCK_NB {4}
    };
}

# instead of Carp.pm
sub carp(@);
sub croak(@);
sub cluck(@);
sub confess(@);

my $__FILE__ = __FILE__;

BEGIN {
    if ($^X =~ m/ jperl /oxmsi) {
        die "$0 need perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
}

my $your_char = q{\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[\x00-\xFF]};

# regexp of character
my $q_char = qr/$your_char/oxms;

#
# EUC-JP character range per length
#
my %range_tr = ();
my $is_shiftjis_family = 0;
my $is_eucjp_family    = 0;

#
# alias of encoding name
#

BEGIN { eval q{ use vars qw($encoding_alias) } }

if (0) {
}

# Latin-1
elsif (__PACKAGE__ eq 'Elatin1') {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: ISO[-_ ]?8859-15? | IEC[- ]?8859-15? | Latin-?1 | Windows-?1252 | ECMA-?94 | ISO_8859-1:1987 | iso-ir-?100 | csISOLatin1 | l1 | IBM819 | CP819 ) \b /oxmsi;
}

# EUC-JP
elsif (__PACKAGE__ eq 'Eeucjp') {
    %range_tr = (
        1 => [ [0x00..0x8D,0x90..0xA0,0xFF],
             ],
        2 => [ [0x8E..0x8E],[0xA1..0xDF],
               [0xA1..0xFE],[0xA1..0xFE],
             ],
        3 => [ [0x8F..0x8F],[0xA1..0xFE],[0xA1..0xFE],
             ],
    );
    $is_eucjp_family = 1;
    $encoding_alias = qr/ \b (?: euc.*jp | jp.*euc | ujis ) \b /oxmsi;
}

# UTF-2
elsif (__PACKAGE__ eq 'Eutf2') {
    %range_tr = (
        1 => [ [0x00..0x7F],
             ],
        2 => [ [0xC2..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xE0],[0xA0..0xBF],[0x80..0xBF],
               [0xE1..0xEC],[0x80..0xBF],[0x80..0xBF],
               [0xED..0xED],[0x80..0x9F],[0x80..0xBF],
               [0xEE..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF0],[0x90..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF1..0xF3],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF4..0xF4],[0x80..0x8F],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: UTF-8 | utf-8-strict | UTF-?2 ) \b /oxmsi;
}

# Old UTF-8
elsif (__PACKAGE__ eq 'Eoldutf8') {
    %range_tr = (
        1 => [ [0x00..0x7F],
             ],
        2 => [ [0xC0..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF4],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: utf8 | CESU-?8 | Modified[ ]?UTF-?8 | Old[ ]?UTF-?8 ) \b /oxmsi;
}

else {
    croak "$0 don't know my package name '" . __PACKAGE__ . "'";
}

#
# Prototypes of subroutines
#
sub import() {}
sub unimport() {}
sub Eeucjp::split(;$$$);
sub Eeucjp::tr($$$$;$);
sub Eeucjp::chop(@);
sub Eeucjp::index($$;$);
sub Eeucjp::rindex($$;$);
sub Eeucjp::capture($);
sub Eeucjp::chr(;$);
sub Eeucjp::chr_();
sub Eeucjp::glob($);
sub Eeucjp::glob_();

sub EUCJP::ord(;$);
sub EUCJP::ord_();
sub EUCJP::reverse(@);
sub EUCJP::length(;$);
sub EUCJP::substr($$;$$);
sub EUCJP::index($$;$);
sub EUCJP::rindex($$;$);

#
# @ARGV wildcard globbing
#
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    if ($ENV{'ComSpec'} =~ / (?: COMMAND\.COM | CMD\.EXE ) \z /oxmsi) {
        my @argv = ();
        for (@ARGV) {
            if (m/\A ' ((?:$q_char)*) ' \z/oxms) {
                push @argv, $1;
            }
            elsif (m/\A (?:$q_char)*? [*?] /oxms and (my @glob = Eeucjp::glob($_))) {
                push @argv, @glob;
            }
            else {
                push @argv, $_;
            }
        }
        @ARGV = @argv;
    }
}

#
# EUC-JP split
#
sub Eeucjp::split(;$$$) {

    # P.794 29.2.161. split
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    my $pattern = $_[0];
    my $string  = $_[1];
    my $limit   = $_[2];

    # if $string is omitted, the function splits the $_ string
    if (not defined $string) {
        if (defined $_) {
            $string = $_;
        }
        else {
            $string = '';
        }
    }

    my @split = ();

    # when string is empty
    if ($string eq '') {

        # resulting list value in list context
        if (wantarray) {
            return @split;
        }

        # count of substrings in scalar context
        else {
            carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
            @_ = @split;
            return scalar @_;
        }
    }

    # if $limit is negative, it is treated as if an arbitrarily large $limit has been specified
    if ((not defined $limit) or ($limit <= 0)) {

        # if $pattern is also omitted or is the literal space, " ", the function splits
        # on whitespace, /\s+/, after skipping any leading whitespace
        # (and so on)

        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;

            # P.1024 Appendix W.10 Multibyte Processing
            # of ISBN 1-56592-224-7 CJKV Information Processing
            # (and so on)

            # the //m modifier is assumed when you split on the pattern /^/
            # (and so on)

            while ($string =~ s/\A((?:$q_char)*?)\s+//m) {

                # if the $pattern contains parentheses, then the substring matched by each pair of parentheses
                # is included in the resulting list, interspersed with the fields that are ordinarily returned
                # (and so on)

                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        # a pattern capable of matching either the null string or something longer than the
        # null string will split the value of $string into separate characters wherever it
        # matches the null string between characters
        # (and so on)

        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        else {
            while ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }
    }

    else {
        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)\s+//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        else {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
    }

    push @split, $string;

    # if $limit is omitted or zero, trailing null fields are stripped from the result
    if ((not defined $limit) or ($limit == 0)) {
        while ((scalar(@split) >= 1) and ($split[-1] eq '')) {
            pop @split;
        }
    }

    # resulting list value in list context
    if (wantarray) {
        return @split;
    }

    # count of substrings in scalar context
    else {
        carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
        @_ = @split;
        return scalar @_;
    }
}

#
# EUC-JP transliteration (tr///)
#
sub Eeucjp::tr($$$$;$) {

    my $bind_operator   = $_[1];
    my $searchlist      = $_[2];
    my $replacementlist = $_[3];
    my $modifier        = $_[4] || '';

    my @char            = $_[0] =~ m/\G ($q_char) /oxmsg;
    my @searchlist      = _charlist_tr($searchlist);
    my @replacementlist = _charlist_tr($replacementlist);

    my %tr = ();
    for (my $i=0; $i <= $#searchlist; $i++) {
        if (not exists $tr{$searchlist[$i]}) {
            if (defined $replacementlist[$i] and ($replacementlist[$i] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[$i];
            }
            elsif ($modifier =~ m/d/oxms) {
                $tr{$searchlist[$i]} = '';
            }
            elsif (defined $replacementlist[-1] and ($replacementlist[-1] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[-1];
            }
            else {
                $tr{$searchlist[$i]} = $searchlist[$i];
            }
        }
    }

    my $tr = 0;
    $_[0] = '';
    if ($modifier =~ m/c/oxms) {
        while (defined(my $char = shift @char)) {
            if (not exists $tr{$char}) {
                if (defined $replacementlist[0]) {
                    $_[0] .= $replacementlist[0];
                }
                $tr++;
                if ($modifier =~ m/s/oxms) {
                    while (@char and (not exists $tr{$char[0]})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $_[0] .= $char;
            }
        }
    }
    else {
        while (defined(my $char = shift @char)) {
            if (exists $tr{$char}) {
                $_[0] .= $tr{$char};
                $tr++;
                if ($modifier =~ m/s/oxms) {
                    while (@char and (exists $tr{$char[0]}) and ($tr{$char[0]} eq $tr{$char})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $_[0] .= $char;
            }
        }
    }

    if ($bind_operator =~ m/ !~ /oxms) {
        return not $tr;
    }
    else {
        return $tr;
    }
}

#
# EUC-JP chop
#
sub Eeucjp::chop(@) {

    my $chop;
    if (@_ == 0) {
        my @char = m/\G ($q_char) /oxmsg;
        $chop = pop @char;
        $_ = join '', @char;
    }
    else {
        for (@_) {
            my @char = m/\G ($q_char) /oxmsg;
            $chop = pop @char;
            $_ = join '', @char;
        }
    }
    return $chop;
}

#
# EUC-JP index by octet
#
sub Eeucjp::index($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= 0;
    my $pos = 0;

    while ($pos < CORE::length($str)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            if ($pos >= $position) {
                return $pos;
            }
        }
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return -1;
}

#
# EUC-JP reverse index
#
sub Eeucjp::rindex($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= CORE::length($str) - 1;
    my $pos = 0;
    my $rindex = -1;

    while (($pos < CORE::length($str)) and ($pos <= $position)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            $rindex = $pos;
        }
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return $rindex;
}

#
# EUC-JP regexp capture
#
{
    # 10.3. Creating Persistent Private Variables
    # in Chapter 10. Subroutines
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

    my $last_s_matched = 0;

    sub Eeucjp::capture($) {
        if ($last_s_matched and ($_[0] =~ m/\A [1-9][0-9]* \z/oxms)) {
            return $_[0] + 1;
        }
        return $_[0];
    }

    # EUC-JP regexp mark last m// or qr// matched
    sub Eeucjp::m_matched() {
        $last_s_matched = 0;
    }

    # EUC-JP regexp mark last s/// or qr matched
    sub Eeucjp::s_matched() {
        $last_s_matched = 1;
    }

    # which matched of m// or s/// at last

    # P.854 31.17. use re
    # in Chapter 31. Pragmatic Modules
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    @Eeucjp::m_matched = (qr/(?{Eeucjp::m_matched})/);
    @Eeucjp::s_matched = (qr/(?{Eeucjp::s_matched})/);
}

#
# prepare EUC-JP characters per length
#

# 1 octet characters
my @chars1 = ();
sub chars1 {
    if (@chars1) {
        return @chars1;
    }
    if (exists $range_tr{1}) {
        my @ranges = @{ $range_tr{1} };
        while (my @range = splice(@ranges,0,1)) {
            for my $oct0 (@{$range[0]}) {
                push @chars1, pack 'C', $oct0;
            }
        }
    }
    return @chars1;
}

# 2 octets characters
my @chars2 = ();
sub chars2 {
    if (@chars2) {
        return @chars2;
    }
    if (exists $range_tr{2}) {
        my @ranges = @{ $range_tr{2} };
        while (my @range = splice(@ranges,0,2)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    push @chars2, pack 'CC', $oct0,$oct1;
                }
            }
        }
    }
    return @chars2;
}

# 3 octets characters
my @chars3 = ();
sub chars3 {
    if (@chars3) {
        return @chars3;
    }
    if (exists $range_tr{3}) {
        my @ranges = @{ $range_tr{3} };
        while (my @range = splice(@ranges,0,3)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        push @chars3, pack 'CCC', $oct0,$oct1,$oct2;
                    }
                }
            }
        }
    }
    return @chars3;
}

# 4 octets characters
my @chars4 = ();
sub chars4 {
    if (@chars4) {
        return @chars4;
    }
    if (exists $range_tr{4}) {
        my @ranges = @{ $range_tr{4} };
        while (my @range = splice(@ranges,0,4)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        for my $oct3 (@{$range[3]}) {
                            push @chars4, pack 'CCCC', $oct0,$oct1,$oct2,$oct3;
                        }
                    }
                }
            }
        }
    }
    return @chars4;
}

# minimum value of each octet
my @minchar = ();
sub minchar {
    if (defined $minchar[$_[0]]) {
        return $minchar[$_[0]];
    }
    $minchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[0];
}

# maximum value of each octet
my @maxchar = ();
sub maxchar {
    if (defined $maxchar[$_[0]]) {
        return $maxchar[$_[0]];
    }
    $maxchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[-1];
}

#
# EUC-JP open character list for tr
#
sub _charlist_tr {

    local $_ = shift @_;

    # unescape character
    my @char = ();
    while (not m/\G \z/oxmsgc) {
        if (m/\G (\\0?55|\\x2[Dd]|\\-) /oxmsgc) {
            push @char, '\-';
        }
        elsif (m/\G \\ ([0-7]{2,3}) /oxmsgc) {
            push @char, CORE::chr(oct $1);
        }
        elsif (m/\G \\x ([0-9A-Fa-f]{1,2}) /oxmsgc) {
            push @char, CORE::chr(hex $1);
        }
        elsif (m/\G \\c ([\x40-\x5F]) /oxmsgc) {
            push @char, CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif (m/\G (\\ [0nrtfbae]) /oxmsgc) {
            push @char, {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
            }->{$1};
        }
        elsif (m/\G \\ ($q_char) /oxmsgc) {
            push @char, $1;
        }
        elsif (m/\G ($q_char) /oxmsgc) {
            push @char, $1;
        }
    }

    # join separated multiple octet
    @char = join('',@char) =~ m/\G (\\-|$q_char) /oxmsg;

    # unescape '-'
    my @i = ();
    for my $i (0 .. $#char) {
        if ($char[$i] eq '\-') {
            $char[$i] = '-';
        }
        elsif ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                push @i, $i;
            }
        }
    }

    # open character list (reverse for splice)
    for my $i (CORE::reverse @i) {
        my @range = ();

        # range error
        if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
            croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
        }

        # range of multiple octet code
        if (length($char[$i-1]) == 1) {
            if (length($char[$i+1]) == 1) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars1();
            }
            elsif (length($char[$i+1]) == 2) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range, grep {$_ le $char[$i+1]}                           &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 2) {
            if (length($char[$i+1]) == 2) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 3) {
            if (length($char[$i+1]) == 3) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 4) {
            if (length($char[$i+1]) == 4) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars4();
            }
        }

        splice @char, $i-1, 3, @range;
    }

    return @char;
}

#
# EUC-JP octet range
#
sub _octets {

    my $modifier = pop @_;
    my $length = shift;

    my($a) = unpack 'C', $_[0];
    my($z) = unpack 'C', $_[1];

    # single octet code
    if ($length == 1) {

        # single octet and ignore case
        if (((caller(1))[3] ne 'Eeucjp::_octets') and ($modifier =~ m/i/oxms)) {
            if ($a == $z) {
                return sprintf('(?i:\x%02X)',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('(?i:[\x%02X\x%02X])',  $a, $z);
            }
            else {
                return sprintf('(?i:[\x%02X-\x%02X])', $a, $z);
            }
        }

        # not ignore case or one of multiple octet
        else {
            if ($a == $z) {
                return sprintf('\x%02X',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('[\x%02X\x%02X]',  $a, $z);
            }
            else {
                return sprintf('[\x%02X-\x%02X]', $a, $z);
            }
        }
    }

    # double octet code of Shift_JIS family
    elsif (($length == 2) and $is_shiftjis_family and ($a <= 0x9F) and (0xE0 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        if ($a == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                            0x9F,$a2);
        }
        elsif (($a+1) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|\x%02X[\x00-\xFF]',          $a,  $a2,$a+1);
        }
        elsif (($a+2) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X\x%02X][\x00-\xFF]',  $a,  $a2,$a+1,$a+2);
        }
        else {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X-\x%02X][\x00-\xFF]', $a,  $a2,$a+1,$a+2);
        }

        if ($z == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xE0) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xE0,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # double octet code of EUC-JP family
    elsif (($length == 2) and $is_eucjp_family and ($a == 0x8E) and (0xA1 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                                0x8E,$a2);

        if ($z == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xA1) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xA1,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # multiple octet code
    else {
        my(undef,$aa) = unpack 'Ca*', $_[0];
        my(undef,$zz) = unpack 'Ca*', $_[1];

        if ($a == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                $zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+1) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+2) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',         $a+1,       _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+3) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X\x%02X]%s', $a+1,$z-1,  _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        else {
            return '(?:' . join('|',
                sprintf('\x%02X%s',          $a,        _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X-\x%02X]%s', $a+1,$z-1, _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',               $z,   _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
    }
}

#
# EUC-JP open character list for qr and not qr
#
sub _charlist {

    my $modifier = pop @_;
    my @char = @_;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {

        # escape - to ...
        if ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                $char[$i] = '...';
            }
        }
        elsif ($char[$i] =~ m/\A \\ ([0-7]{2,3}) \z/oxms) {
            $char[$i] = CORE::chr oct $1;
        }
        elsif ($char[$i] =~ m/\A \\x ([0-9A-Fa-f]{1,2}) \z/oxms) {
            $char[$i] = CORE::chr hex $1;
        }
        elsif ($char[$i] =~ m/\A \\c ([\x40-\x5F]) \z/oxms) {
            $char[$i] = CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif ($char[$i] =~ m/\A (\\ [0nrtfbaedDhHsSvVwW]) \z/oxms) {
            $char[$i] = {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
                '\d' => '[0-9]',
                '\s' => '[\x09\x0A\x0C\x0D\x20]',
                '\w' => '[0-9A-Z_a-z]',
                '\D' => '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^0-9])',
                '\S' => '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x09\x0A\x0C\x0D\x20])',
                '\W' => '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^0-9A-Z_a-z])',

                '\H' => '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x09\x20])',
                '\V' => '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^\x0C\x0A\x0D])',
                '\h' => '[\x09\x20]',
                '\v' => '[\x0C\x0A\x0D]',

            }->{$1};
        }
        elsif ($char[$i] =~ m/\A \\ ($q_char) \z/oxms) {
            $char[$i] = $1;
        }
    }

    # open character list
    my @singleoctet = ();
    my @charlist    = ();
    for (my $i=0; $i <= $#char; ) {

        # escaped -
        if (defined($char[$i+1]) and ($char[$i+1] eq '...')) {
            $i += 1;
            next;
        }
        elsif ($char[$i] eq '...') {

            # range error
            if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            # range of single octet code and not ignore case
            if ((length($char[$i-1]) == 1) and (length($char[$i+1]) == 1) and ($modifier !~ m/i/oxms)) {
                my $a = unpack 'C', $char[$i-1];
                my $z = unpack 'C', $char[$i+1];

                if ($a == $z) {
                    push @singleoctet, sprintf('\x%02X',        $a);
                }
                elsif (($a+1) == $z) {
                    push @singleoctet, sprintf('\x%02X\x%02X',  $a, $z);
                }
                else {
                    push @singleoctet, sprintf('\x%02X-\x%02X', $a, $z);
                }
            }

            # range of multiple octet code
            elsif (length($char[$i-1]) == length($char[$i+1])) {
                push @charlist, _octets(length($char[$i-1]), $char[$i-1], $char[$i+1], $modifier);
            }
            elsif (length($char[$i-1]) == 1) {
                if (length($char[$i+1]) == 2) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 2) {
                if (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 3) {
                if (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(3, $char[$i-1], &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            else {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            $i += 2;
        }

        # /i modifier
        elsif ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) {
            if ($modifier =~ m/i/oxms) {
                my $uc = uc($char[$i]);
                my $lc = lc($char[$i]);
                if ($uc ne $lc) {
                    push @singleoctet, $uc, $lc;
                }
                else {
                    push @singleoctet, $char[$i];
                }
            }
            else {
                push @singleoctet, $char[$i];
            }
            $i += 1;
        }

        # single character of single octet code

        # \h \v
        #
        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        elsif ($char[$i] =~ m/\A (?: \\h ) \z/oxms) {
            push @singleoctet, "\t", "\x20";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: \\v ) \z/oxms) {
            push @singleoctet, "\f","\n","\r";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: [\x00-\xFF] | \\d | \\s | \\w ) \z/oxms) {
            push @singleoctet, $char[$i];
            $i += 1;
        }

        # single character of multiple octet code
        else {
            push @charlist, $char[$i];
            $i += 1;
        }
    }

    # quote metachar
    for (@singleoctet) {
        if (m/\A \n \z/oxms) {
            $_ = '\n';
        }
        elsif (m/\A \r \z/oxms) {
            $_ = '\r';
        }
        elsif (m/\A ([\x00-\x20\x7F-\xFF]) \z/oxms) {
            $_ = sprintf('\x%02X', CORE::ord $1);
        }
        elsif (m/\A [\x00-\xFF] \z/oxms) {
            $_ = quotemeta $_;
        }
    }

    # return character list
    return \@singleoctet, \@charlist;
}

#
# EUC-JP open character list for qr
#
sub charlist_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@singleoctet) == 0) {
    }
    elsif (scalar(@singleoctet) >= 2) {
        push @charlist, '[' . join('',@singleoctet) . ']';
    }
    elsif ($singleoctet[0] =~ m/ . - . /oxms) {
        push @charlist, '[' . $singleoctet[0] . ']';
    }
    else {
        push @charlist, $singleoctet[0];
    }
    if (scalar(@charlist) >= 2) {
        return '(?:' . join('|', @charlist) . ')';
    }
    else {
        return $charlist[0];
    }
}

#
# EUC-JP open character list for not qr
#
sub charlist_not_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@charlist) >= 1) {
        if (scalar(@singleoctet) >= 1) {

            # any character other than multiple octet and single octet character class
            return '(?!' . join('|', @charlist) . ')(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^'. join('', @singleoctet) . '])';
        }
        else {

            # any character other than multiple octet character class
            return '(?!' . join('|', @charlist) . ")(?:$your_char)";
        }
    }
    else {
        if (scalar(@singleoctet) >= 1) {

            # any character other than single octet character class
            return                                 '(?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^'. join('', @singleoctet) . '])';
        }
        else {

            # any character
            return                                 "(?:$your_char)";
        }
    }
}

#
# EUC-JP order to character (with parameter)
#
sub Eeucjp::chr(;$) {

    my $c = @_ ? $_[0] : $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# EUC-JP order to character (without parameter)
#
sub Eeucjp::chr_() {

    my $c = $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# EUC-JP path globbing (with parameter)
#
sub Eeucjp::glob($) {

    return _dosglob(@_);
}

#
# EUC-JP path globbing (without parameter)
#
sub Eeucjp::glob_() {

    return _dosglob();
}

#
# EUC-JP path globbing from File::DosGlob module
#
my %iter;
my %entries;
sub _dosglob {

    # context (keyed by second cxix argument provided by core)
    my($expr,$cxix) = @_;

    # glob without args defaults to $_
    $expr = $_ if not defined $expr;

    # represents the current user's home directory
    #
    # 7.3. Expanding Tildes in Filenames
    # in Chapter 7. File Access
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
    #
    # and File::HomeDir, File::HomeDir::Windows module

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $expr =~ s{ \A ~ (?= [^/\\] ) }
                  { $ENV{'HOME'} || $ENV{'USERPROFILE'} || "$ENV{'HOMEDRIVE'}$ENV{'HOMEPATH'}" }oxmse;
    }

    # UNIX-like system
    else {
        $expr =~ s{ \A ~ ( (?:\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^/])* ) }
                  { $1 ? (getpwnam($1))[7] : ($ENV{'HOME'} || $ENV{'LOGDIR'} || (getpwuid($<))[7]) }oxmse;
    }

    # assume global context if not provided one
    $cxix = '_G_' if not defined $cxix;
    $iter{$cxix} = 0 if not exists $iter{$cxix};

    # if we're just beginning, do it all first
    if ($iter{$cxix} == 0) {
            $entries{$cxix} = [ _do_glob(1, _parse_line($expr)) ];
    }

    # chuck it all out, quick or slow
    if (wantarray) {
        delete $iter{$cxix};
        return @{delete $entries{$cxix}};
    }
    else {
        if ($iter{$cxix} = scalar @{$entries{$cxix}}) {
            return shift @{$entries{$cxix}};
        }
        else {
            # return undef for EOL
            delete $iter{$cxix};
            delete $entries{$cxix};
            return undef;
        }
    }
}

#
# EUC-JP path globbing subroutine
#
sub _do_glob {

    my($cond,@expr) = @_;
    my @glob = ();

OUTER:
    for my $expr (@expr) {
        next OUTER if not defined $expr;
        next OUTER if $expr eq '';

        my @matched = ();
        my @globdir = ();
        my $head    = '.';
        my $pathsep = '/';
        my $tail;

        # if argument is within quotes strip em and do no globbing
        if ($expr =~ m/\A " ((?:$q_char)*) " \z/oxms) {
            $expr = $1;
            if ($cond eq 'd') {
                if (-d $expr) {
                    push @glob, $expr;
                }
            }
            else {
                if (-e $expr) {
                    push @glob, $expr;
                }
            }
            next OUTER;
        }

        # wildcards with a drive prefix such as h:*.pm must be changed
        # to h:./*.pm to expand correctly
        if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
            $expr =~ s# \A ((?:[A-Za-z]:)?) (\x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^/\\]) #$1./$2#oxms;
        }

        if (($head, $tail) = _parse_path($expr,$pathsep)) {
            if ($tail eq '') {
                push @glob, $expr;
                next OUTER;
            }
            if ($head =~ m/ \A (?:$q_char)*? [*?] /oxms) {
                if (@globdir = _do_glob('d', $head)) {
                    push @glob, _do_glob($cond, map {"$_$pathsep$tail"} @globdir);
                    next OUTER;
                }
            }
            if ($head eq '' or $head =~ m/\A [A-Za-z]: \z/oxms) {
                $head .= $pathsep;
            }
            $expr = $tail;
        }

        # If file component has no wildcards, we can avoid opendir
        if ($expr !~ m/ \A (?:$q_char)*? [*?] /oxms) {
            if ($head eq '.') {
                $head = '';
            }
            if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
                $head .= $pathsep;
            }
            $head .= $expr;
            if ($cond eq 'd') {
                if (-d $head) {
                    push @glob, $head;
                }
            }
            else {
                if (-e $head) {
                    push @glob, $head;
                }
            }
            next OUTER;
        }
        opendir(*DIR, $head) or next OUTER;
        my @leaf = readdir DIR;
        closedir DIR;

        if ($head eq '.') {
            $head = '';
        }
        if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
            $head .= $pathsep;
        }

        my $pattern = '';
        while ($expr =~ m/ \G ($q_char) /oxgc) {
            my $char = $1;
            if ($char eq '*') {
                $pattern .= "(?:$your_char)*",
            }
            elsif ($char eq '?') {
                $pattern .= "(?:$your_char)?",  # DOS style
#               $pattern .= "(?:$your_char)",   # UNIX style
            }
            elsif ((my $uc = uc($char)) ne $char) {
                $pattern .= $uc;
            }
            else {
                $pattern .= quotemeta $char;
            }
        }
        my $matchsub = sub { uc($_[0]) =~ m{\A $pattern \z}xms };

#       if ($@) {
#           print STDERR "$0: $@\n";
#           next OUTER;
#       }

INNER:
        for my $leaf (@leaf) {
            if ($leaf eq '.' or $leaf eq '..') {
                next INNER;
            }
            if ($cond eq 'd' and not -d "$head$leaf") {
                next INNER;
            }

            if (&$matchsub($leaf)) {
                push @matched, "$head$leaf";
                next INNER;
            }

            # [DOS compatibility special case]
            # Failed, add a trailing dot and try again, but only...

            if (Eeucjp::index($leaf,'.') == -1 and   # if name does not have a dot in it *and*
                CORE::length($leaf) <= 8 and        # name is shorter than or equal to 8 chars *and*
                Eeucjp::index($pattern,'\\.') != -1  # pattern has a dot.
            ) {
                if (&$matchsub("$leaf.")) {
                    push @matched, "$head$leaf";
                    next INNER;
                }
            }
        }
        if (@matched) {
            push @glob, @matched;
        }
    }
    return @glob;
}

#
# EUC-JP parse line
#
sub _parse_line {

    my($line) = @_;

    $line .= ' ';
    my @piece = ();
    while ($line =~ m{
        " ( (?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^"]   )*  ) " \s+ |
          ( (?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^"\s] )*  )   \s+
        }oxmsg
    ) {
        push @piece, defined($1) ? $1 : $2;
    }
    return @piece;
}

#
# EUC-JP parse path
#
sub _parse_path {

    my($path,$pathsep) = @_;

    $path .= '/';
    my @subpath = ();
    while ($path =~ m{
        ((?: \x8F[\xA1-\xFE][\xA1-\xFE]|[\x8E\xA1-\xFE][\x00-\xFF]|[^/\\] )+?) [/\\] }oxmsg
    ) {
        push @subpath, $1;
    }

    my $tail = pop @subpath;
    my $head = join $pathsep, @subpath;
    return $head, $tail;
}

#
# instead of binmode (for perl5.005 only)
#
sub Eeucjp::binmode(*;$) {
    if (@_ == 1) {
        local $^W = 0;
        if (ref $_[0]) {
            my $filehandle = qualify_to_ref $_[0];
            return CORE::binmode $filehandle;
        }
        else {
            return CORE::binmode *{(caller(1))[0] . "::$_[0]"};
        }
    }
    elsif (@_ == 2) {
        my(undef,$layer) = @_;
        $layer =~ s/ :? encoding\($encoding_alias\) //oxms;
        if ($layer =~ m/\A :raw \z/oxms) {
            local $^W = 0;
            if ($_[0] =~ m/\A (?: STDIN | STDOUT | STDERR ) \z/oxms) {
                return CORE::binmode $_[0];
            }
            elsif (ref $_[0]) {
                my $filehandle = qualify_to_ref $_[0];
                return CORE::binmode $filehandle;
            }
            else {
                return CORE::binmode *{(caller(1))[0] . "::$_[0]"};
            }
        }
        elsif ($layer =~ m/\A :crlf \z/oxms) {
            return;
        }
        else {
            return;
        }
    }
    else {
        croak "$0: usage: binmode(FILEHANDLE [,LAYER])";
    }
}

#
# instead of open (for perl5.005 only)
#
sub Eeucjp::open(*;$@) {

    if (@_ == 0) {
        croak "$0: usage: open(FILEHANDLE [,MODE [,EXPR]])";
    }
    elsif (@_ == 1) {
        my $filehandle = gensym;
        local $^W = 0;
        my $expr = ${(caller(1))[0] . "::$_[0]"};
        my $ref = \${(caller(1))[0] . "::$_[0]"};
        *{(caller(1))[0] . "::$_[0]"} = $filehandle;
        *{(caller(1))[0] . "::$_[0]"} = $ref;
        return CORE::open $filehandle, $expr;
    }

    my $filehandle = gensym;
    {
        local $^W = 0;
        if (not defined $_[0]) {
            $_[0] = $filehandle;
        }
        else {
            *{(caller(1))[0] . "::$_[0]"} = $filehandle;
        }
    }

    if (@_ == 2) {
        return CORE::open $filehandle, $_[1];
    }
    elsif (@_ == 3) {
        my(undef,$mode,$expr) = @_;

        $mode =~ s/ :? encoding\($encoding_alias\) //oxms;
        $mode =~ s/ :crlf //oxms;
        my $binmode = $mode =~ s/ :raw //oxms;

        if (eval q{ use Fcntl qw(O_RDONLY O_WRONLY O_RDWR O_CREAT O_TRUNC O_APPEND); 1 }) {

            # 7.1. Opening a File
            # in Chapter 7. File Access
            # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

            my %o_flags = (
                ''    => &O_RDONLY,
                '<'   => &O_RDONLY,
                '>'   => &O_WRONLY | &O_TRUNC  | &O_CREAT,
                '>>'  => &O_WRONLY | &O_APPEND | &O_CREAT,
                '+<'  => &O_RDWR,
                '+>'  => &O_RDWR   | &O_TRUNC  | &O_CREAT,
                '+>>' => &O_RDWR   | &O_APPEND | &O_CREAT,
            );
            if ($o_flags{$mode}) {
                my $sysopen = CORE::sysopen $filehandle, $expr, $o_flags{$mode};
                if ($sysopen and $binmode) {
                    CORE::binmode $filehandle;
                }
                return $sysopen;
            }
        }

        # P.747 29.2.104. open
        # in Chapter 29: Functions
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.
        # (and so on)

        if ($mode eq '|-') {
            my $open = CORE::open $filehandle, qq{| $expr};
            if ($open and $binmode) {
                CORE::binmode $filehandle;
            }
            return $open;
        }
        elsif ($mode eq '-|') {
            my $open = CORE::open $filehandle, qq{$expr |};
            if ($open and $binmode) {
                CORE::binmode $filehandle;
            }
            return $open;
        }
        elsif ($mode =~ m/\A (?: \+? (?: < | > | >> ) )? \z/oxms) {

            # 7.2. Opening Files with Unusual Filenames
            # in Chapter 7. File Access
            # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.

            $expr =~ s#\A([ ])#./$1#oxms;
            my $open = CORE::open $filehandle, qq{$mode $expr\0};
            if ($open and $binmode) {
                CORE::binmode $filehandle;
            }
            return $open;
        }
        else {
            croak "$0: open: Unknown open() mode '$mode'";
        }
    }
    else {
        croak "$0: usage: open(FILEHANDLE [,MODE [,EXPR]])";
    }
}

#
# EUC-JP character to order (with parameter)
#
sub EUCJP::ord(;$) {

    local $_ = shift if @_;

    if (m/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# EUC-JP character to order (without parameter)
#
sub EUCJP::ord_() {

    if (m/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# EUC-JP reverse
#
sub EUCJP::reverse(@) {

    if (wantarray) {
        return CORE::reverse @_;
    }
    else {
        return join '', CORE::reverse(join('',@_) =~ m/\G ($q_char) /oxmsg);
    }
}

#
# EUC-JP length by character
#
sub EUCJP::length(;$) {

    local $_ = shift if @_;

    local @_ = m/\G ($q_char) /oxmsg;
    return scalar @_;
}

#
# EUC-JP substr by character
#
sub EUCJP::substr($$;$$) {

    my @char = $_[0] =~ m/\G ($q_char) /oxmsg;

    # substr($string,$offset,$length,$replacement)
    if (@_ == 4) {
        my(undef,$offset,$length,$replacement) = @_;
        my $substr = join '', splice(@char, $offset, $length, $replacement);
        $_[0] = join '', @char;
        return $substr;
    }

    # substr($string,$offset,$length)
    elsif (@_ == 3) {
        my(undef,$offset,$length) = @_;
        if ($length == 0) {
            return '';
        }
        if ($offset >= 0) {
            return join '', (@char[$offset            .. $#char])[0 .. $length-1];
        }
        else {
            return join '', (@char[($#char+$offset+1) .. $#char])[0 .. $length-1];
        }
    }

    # substr($string,$offset)
    else {
        my(undef,$offset) = @_;
        if ($offset >= 0) {
            return join '', @char[$offset            .. $#char];
        }
        else {
            return join '', @char[($#char+$offset+1) .. $#char];
        }
    }
}

#
# EUC-JP index by character
#
sub EUCJP::index($$;$) {

    my $index;
    if (@_ == 3) {
        $index = Eeucjp::index($_[0], $_[1], CORE::length(EUCJP::substr($_[0], 0, $_[2])));
    }
    else {
        $index = Eeucjp::index($_[0], $_[1]);
    }

    if ($index == -1) {
        return -1;
    }
    else {
        return EUCJP::length(CORE::substr $_[0], 0, $index);
    }
}

#
# EUC-JP rindex by character
#
sub EUCJP::rindex($$;$) {

    my $rindex;
    if (@_ == 3) {
        $rindex = Eeucjp::rindex($_[0], $_[1], CORE::length(EUCJP::substr($_[0], 0, $_[2])));
    }
    else {
        $rindex = Eeucjp::rindex($_[0], $_[1]);
    }

    if ($rindex == -1) {
        return -1;
    }
    else {
        return EUCJP::length(CORE::substr $_[0], 0, $rindex);
    }
}

#
# instead of Carp::carp
#
sub carp(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
}

#
# instead of Carp::croak
#
sub croak(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
    die "\n";
}

#
# instead of Carp::cluck
#
sub cluck(@) {
    my $i = 0;
    my @cluck = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @cluck, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @cluck;
    print STDERR "\n";
    carp @_;
}

#
# instead of Carp::confess
#
sub confess(@) {
    my $i = 0;
    my @confess = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @confess, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @confess;
    print STDERR "\n";
    croak @_;
}

1;

__END__

=pod

=head1 NAME

Eeucjp - Run-time routines for EUCJP.pm

=head1 SYNOPSIS

  use Eeucjp;

    Eeucjp::split(...);
    Eeucjp::tr(...);
    Eeucjp::chop(...);
    Eeucjp::index(...);
    Eeucjp::rindex(...);
    Eeucjp::capture(...);
    Eeucjp::chr(...);
    Eeucjp::chr_;
    Eeucjp::glob(...);
    Eeucjp::glob_;

  # "no Eeucjp;" not supported

=head1 ABSTRACT

This module is a run-time routines of the EUCJP module.
Because the EUCJP module automatically uses this module, you need not use directly.

=head1 BUGS AND LIMITATIONS

Please patches and report problems to author are welcome.

=head1 HISTORY

This Eeucjp module first appeared in ActivePerl Build 522 Built under
MSWin32 Compiled at Nov 2 1999 09:52:28

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.
For any questions, use E<lt>ina@cpan.orgE<gt> so we can share
this file.

=head1 LICENSE AND COPYRIGHT

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 EXAMPLES

=over 2

=item Split string

  @split = Eeucjp::split(/pattern/,$string,$limit);
  @split = Eeucjp::split(/pattern/,$string);
  @split = Eeucjp::split(/pattern/);
  @split = Eeucjp::split('',$string,$limit);
  @split = Eeucjp::split('',$string);
  @split = Eeucjp::split('');
  @split = Eeucjp::split();
  @split = Eeucjp::split;

  Scans a EUC-JP $string for delimiters that match pattern and splits the EUC-JP
  $string into a list of substrings, returning the resulting list value in list
  context, or the count of substrings in scalar context. The delimiters are
  determined by repeated pattern matching, using the regular expression given in
  pattern, so the delimiters may be of any size and need not be the same EUC-JP
  $string on every match. If the pattern doesn't match at all, Eeucjp::split returns
  the original EUC-JP $string as a single substring. If it matches once, you get
  two substrings, and so on.
  If $limit is specified and is not negative, the function splits into no more than
  that many fields. If $limit is negative, it is treated as if an arbitrarily large
  $limit has been specified. If $limit is omitted, trailing null fields are stripped
  from the result (which potential users of pop would do well to remember).
  If EUC-JP $string is omitted, the function splits the $_ EUC-JP string.
  If $patten is also omitted, the function splits on whitespace, /\s+/, after
  skipping any leading whitespace.
  If the pattern contains parentheses, then the substring matched by each pair of
  parentheses is included in the resulting list, interspersed with the fields that
  are ordinarily returned.

=item Transliteration

  $tr = Eeucjp::tr($variable,$bind_operator,$searchlist,$replacementlist,$modifier);
  $tr = Eeucjp::tr($variable,$bind_operator,$searchlist,$replacementlist);

  This function scans a EUC-JP string character by character and replaces all
  occurrences of the characters found in $searchlist with the corresponding character
  in $replacementlist. It returns the number of characters replaced or deleted.
  If no EUC-JP string is specified via =~ operator, the $_ variable is translated.
  $modifier are:

  Modifier   Meaning
  ------------------------------------------------------
  c          Complement $searchlist
  d          Delete found but unreplaced characters
  s          Squash duplicate replaced characters
  ------------------------------------------------------

=item Chop string

  $chop = Eeucjp::chop(@list);
  $chop = Eeucjp::chop();
  $chop = Eeucjp::chop;

  Chops off the last character of a EUC-JP string contained in the variable (or
  EUC-JP strings in each element of a @list) and returns the character chopped.
  The Eeucjp::chop operator is used primarily to remove the newline from the end of
  an input record but is more efficient than s/\n$//. If no argument is given, the
  function chops the $_ variable.

=item Index string

  $pos = Eeucjp::index($string,$substr,$position);
  $pos = Eeucjp::index($string,$substr);

  Returns the position of the first occurrence of $substr in EUC-JP $string.
  The start, if specified, specifies the $position to start looking in the EUC-JP
  $string. Positions are integer numbers based at 0. If the substring is not found,
  the Eeucjp::index function returns -1.

=item Reverse index string

  $pos = Eeucjp::rindex($string,$substr,$position);
  $pos = Eeucjp::rindex($string,$substr);

  Works just like Eeucjp::index except that it returns the position of the last
  occurence of $substr in EUC-JP $string (a reverse index). The function returns
  -1 if not found. $position, if specified, is the rightmost position that may be
  returned, i.e., how far in the EUC-JP string the function can search.

=item Make capture number

  $capturenumber = Eeucjp::capture($string);

  This function is internal use to m/ /i, s/ / /i, split and qr/ /i.

=item Make character

  $chr = Eeucjp::chr($code);
  $chr = Eeucjp::chr_;

  This function returns the character represented by that $code in the character
  set. For example, Eeucjp::chr(65) is "A" in either ASCII or EUC-JP, and
  Eeucjp::chr(0x82a0) is a EUC-JP HIRAGANA LETTER A. For the reverse of Eeucjp::chr,
  use EUCJP::ord.

=item Filename expansion (globbing)

  @glob = Eeucjp::glob($string);
  @glob = Eeucjp::glob_;

  Performs filename expansion (DOS-like globbing) on $string, returning the next
  successive name on each call. If $string is omitted, $_ is globbed instead.
  This function function when the pathname ends with chr(0x5C) on MSWin32.

  For example, C<<..\\l*b\\file/*glob.p?>> on MSWin32 or UNIX will work as
  expected (in that it will find something like '..\lib\File/DosGlob.pm'
  alright).
  Note that all path components are
  case-insensitive, and that backslashes and forward slashes are both accepted,
  and preserved. You may have to double the backslashes if you are putting them in
  literally, due to double-quotish parsing of the pattern by perl.
  A tilde ("~") expands to the current user's home directory.

  Spaces in the argument delimit distinct patterns, so C<glob('*.exe *.dll')> globs
  all filenames that end in C<.exe> or C<.dll>. If you want to put in literal spaces
  in the glob pattern, you can escape them with either double quotes.
  e.g. C<glob('c:/"Program Files"/*/*.dll')>.

=back

=cut


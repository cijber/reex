use strict;
use warnings FATAL => 'all';
use Fcntl qw(SEEK_SET SEEK_CUR SEEK_END);

sub main {
    my $file = shift;
    open(my $reexasm, $file) or die("can't open $file");
    my @asm;
    my %labels;
    my $line_index = 0;
    while (my $line = <$reexasm>) {
        if ($line =~ /^#/) {
            next
        }

        if (my ($label) = ($line =~ /([0-9_A-Z]+):/)) {
            $labels{$label} = $line_index;
            next;
        }

        $line_index += 1;
    }
    seek $reexasm, 0, SEEK_SET;
    while (my $line = <$reexasm>) {
        if ($line =~ /^#/ || $line =~ /([0-9_A-Z]+):/) {
            next
        }

        if (my ($label) = ($line =~ /jmp\s+([A-Z0-9_]+)/)) {
            print "jmp $labels{$label};\n";
            next;
        }

        if (my ($label, $label_sec) = ($line =~ /split\s+([A-Z0-9_]+),\s*([A-Z0-9_]+)/)) {
            print "split $labels{$label}, $labels{$label_sec};\n";
            next;
        }

        $line =~ s/^\s+//;
        $line =~ s/\n$//;
        print "$line;\n"
    }
}

use File::Basename;
my $dirname = dirname(__FILE__);

main "$dirname/benbe.txt"
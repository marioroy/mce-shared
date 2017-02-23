###############################################################################
## ----------------------------------------------------------------------------
## Handle helper class.
##
###############################################################################

package MCE::Shared::Handle;

use 5.010001;
use strict;
use warnings;

no warnings qw( threads recursion uninitialized numeric );

our $VERSION = '1.813';

## no critic (InputOutput::ProhibitTwoArgOpen)
## no critic (Subroutines::ProhibitSubroutinePrototypes)
## no critic (TestingAndDebugging::ProhibitNoStrict)

use MCE::Shared::Base;
use bytes;

sub import {
   if (!exists $INC{'MCE/Shared.pm'}) {
      no strict 'refs'; no warnings 'redefine';
      *{ caller().'::mce_open' } = \&open;
   }
   return;
}

sub _croak {
   goto &MCE::Shared::Base::_croak;
}

sub TIEHANDLE {
   my $class = shift;

   if (ref $_[0] eq 'ARRAY') {
      # For use with MCE::Shared in order to reach the Server process.
      # Basically, without a GLOB initially.
      bless $_[0], $class;
   }
   else {
      my $fh = \do { no warnings 'once'; local *FH };
      bless $fh, $class;

      if (@_ == 2 && ref $_[1] && defined(my $_fd = fileno($_[1]))) {
         $fh->OPEN($_[0]."&=$_fd") or _croak("open error: $!");
      } elsif (@_) {
         $fh->OPEN(@_) or _croak("open error: $!");
      }

      $fh;
   }
}

###############################################################################
## ----------------------------------------------------------------------------
## Based on Tie::StdHandle.
##
###############################################################################

sub EOF     { eof($_[0]) }
sub TELL    { tell($_[0]) }
sub FILENO  { fileno($_[0]) }
sub SEEK    { seek($_[0], $_[1], $_[2]) }
sub CLOSE   { close($_[0]) }
sub BINMODE { binmode($_[0]) }
sub GETC    { getc($_[0]) }

sub OPEN {
   $_[0]->CLOSE if defined ( $_[0]->FILENO );
   @_ == 2
      ? CORE::open($_[0], $_[1])
      : CORE::open($_[0], $_[1], $_[2]);
}

sub open (@) {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared::Handle' );
   my $item;

   if ( ref $_[0] eq 'GLOB' && tied *{ $_[0] } &&
        ref tied(*{ $_[0] }) eq __PACKAGE__ ) {
      $item = tied *{ $_[0] };
   }
   elsif ( @_ ) {
      if ( ref $_[0] eq 'GLOB' && tied *{ $_[0] } ) {
         close $_[0] if defined ( fileno $_[0] );
      }
      $_[0] = \do { no warnings 'once'; local *FH };
      $item = tie *{ $_[0] }, __PACKAGE__;
   }

   shift; _croak("Not enough arguments for open") unless @_;

   if ( !defined wantarray ) {
      $item->OPEN(@_) or _croak("open error: $!");
   } else {
      $item->OPEN(@_);
   }
}

sub READ {
   my ($fh, $len, $auto) = ($_[0], $_[2]);

   if (lc(substr $len, -1, 1) eq 'm') {
      $auto = 1;  chop $len;  $len *= 1024 * 1024;
   } elsif (lc(substr $len, -1, 1) eq 'k') {
      $auto = 1;  chop $len;  $len *= 1024;
   }

   # normal use-case

   if (!$auto) {
      return @_ == 4 ? read($fh, $_[1], $len, $_[3]) : read($fh, $_[1], $len);
   }

   # chunk IO, read up to record separator or eof
   # support special case; e.g. $/ = "\n>" for bioinformatics
   # anchoring ">" at the start of line

   my ($tmp, $ret);

   if (!eof($fh)) {
      if (length $/ > 1 && substr($/, 0, 1) eq "\n") {
         my $len = length($/) - 1;

         if (tell $fh) {
            $tmp = substr($/, 1);
            $ret = read($fh, $tmp, $len, length($tmp));
         } else {
            $ret = read($fh, $tmp, $len);
         }

         if (defined $ret) {
            $.   += 1 if eof($fh);
            $tmp .= readline($fh);

            substr($tmp, -$len, $len, '')
               if (substr($tmp, -$len) eq substr($/, 1));
         }
      }
      elsif (defined ($ret = CORE::read($fh, $tmp, $len))) {
         $.   += 1 if eof($fh);
         $tmp .= readline($fh);
      }
   }
   else {
      $tmp = '', $ret = 0;
   }

   if (defined $ret) {
      my $pos = $_[3] || 0;
      substr($_[1], $pos, length($_[1]) - $pos, $tmp);
      length($tmp);
   }
   else {
      undef;
   }
}

sub READLINE {
   # support special case; e.g. $/ = "\n>" for bioinformatics
   # anchoring ">" at the start of line

   if (length $/ > 1 && substr($/, 0, 1) eq "\n" && !eof($_[0])) {
      my ($len, $buf) = (length($/) - 1);

      if (tell $_[0]) {
         $buf = substr($/, 1), $buf .= readline($_[0]);
      } else {
         $buf = readline($_[0]);
      }

      substr($buf, -$len, $len, '')
         if (substr($buf, -$len) eq substr($/, 1));

      $buf;
   }
   else {
      scalar(readline($_[0]));
   }
}

sub PRINT {
   my $fh  = shift;
   my $buf = join(defined $, ? $, : "", @_);
   $buf   .= $\ if defined $\;
   local $\; # don't print any line terminator
   print $fh $buf;
}

sub PRINTF {
   my $fh  = shift;
   my $buf = sprintf(shift, @_);
   local $\; # ditto
   print $fh $buf;
}

sub WRITE {
   @_ > 2 ? syswrite($_[0], $_[1], $_[2], $_[3] || 0)
          : syswrite($_[0], $_[1]);
}

1;

__END__

###############################################################################
## ----------------------------------------------------------------------------
## Module usage.
##
###############################################################################

=head1 NAME

MCE::Shared::Handle - Handle helper class

=head1 VERSION

This document describes MCE::Shared::Handle version 1.813

=head1 DESCRIPTION

A handle helper class for use as a standalone or managed by L<MCE::Shared>.

=head1 SYNOPSIS

   # non-shared or local construction for use by a single process

   use MCE::Shared::Handle;

   MCE::Shared::Handle->open( my $fh, "<", "bio.fasta" );
   MCE::Shared::Handle::open  my $fh, "<", "bio.fasta";

   mce_open my $fh, "<", "bio.fasta" or die "open error: $!";

   # construction for sharing with other threads and processes

   use MCE::Shared;

   MCE::Shared->open( my $fh, "<", "bio.fasta" );
   MCE::Shared::open  my $fh, "<", "bio.fasta";

   mce_open my $fh, "<", "bio.fasta" or die "open error: $!";

   # example, output is serialized, not garbled

   use MCE::Hobo;
   use MCE::Shared;

   mce_open my $ofh, ">>", \*STDOUT  or die "open error: $!";
   mce_open my $ifh, "<", "file.log" or die "open error: $!";

   sub parallel {
      $/ = "\n"; # can set the input record separator
      while (my $line = <$ifh>) {
         printf {$ofh} "[%5d] %s", $., $line;
      }
   }

   MCE::Hobo->create( \&parallel ) for 1 .. 4;

   $_->join() for MCE::Hobo->list();

   # handle functions

   my $bool = eof($ifh);
   my $off  = tell($ifh);
   my $fd   = fileno($ifh);
   my $char = getc($ifh);
   my $line = readline($ifh);

   binmode $ifh;
   seek $ifh, 10, 0;
   read $ifh, my($buf), 80;

   print  {$ofh} "foo\n";
   printf {$ofh} "%s\n", "bar";

   open $ofh, ">>", \*STDERR;
   syswrite $ofh, "shared handle to STDERR\n";

   close $ifh;
   close $ofh;

=head1 API DOCUMENTATION

=over 3

=item open ( filehandle, expr )

=item open ( filehandle, mode, expr )

=item open ( filehandle, mode, reference )

In version 1.007 and later, constructs a new object by opening the file
whose filename is given by C<expr>, and associates it with C<filehandle>.
When omitting error checking at the application level, MCE::Shared emits
a message and stop if open fails.

   # non-shared or local construction for use by a single process

   use MCE::Shared::Handle;

   MCE::Shared::Handle->open( my $fh, "<", "file.log" ) or die "$!";
   MCE::Shared::Handle::open  my $fh, "<", "file.log"   or die "$!";

   mce_open my $fh, "<", "file.log" or die "$!"; # ditto

   # construction for sharing with other threads and processes

   use MCE::Shared;

   MCE::Shared->open( my $fh, "<", "file.log" ) or die "$!";
   MCE::Shared::open  my $fh, "<", "file.log"   or die "$!";

   mce_open my $fh, "<", "file.log" or die "$!"; # ditto

Simple examples to open a file for reading:

   # mce_open is exported by MCE::Shared or MCE::Shared::Handle.
   # It creates a shared file handle with MCE::Shared present
   # or a non-shared handle otherwise.

   mce_open my $fh, "< input.txt"     or die "open error: $!";
   mce_open my $fh, "<", "input.txt"  or die "open error: $!";
   mce_open my $fh, "<", \*STDIN      or die "open error: $!";

and for writing:

   mce_open my $fh, "> output.txt"    or die "open error: $!";
   mce_open my $fh, ">", "output.txt" or die "open error: $!";
   mce_open my $fh, ">", \*STDOUT     or die "open error: $!";

=back

=head1 CHUNK IO

Starting with C<MCE::Shared> v1.007, chunk IO is possible for both non-shared
and shared handles. Chunk IO is enabled by the trailing 'k' or 'm' for read
size. Also, chunk IO supports the special "\n>"-like record separator.
That anchors ">" at the start of the line. Workers receive record(s) beginning
with ">" and ending with "\n".

   # non-shared handle ---------------------------------------------

   use MCE::Shared::Handle;

   mce_open my $fh, '<', 'bio.fasta' or die "open error: $!";

   # shared handle -------------------------------------------------

   use MCE::Shared;

   mce_open my $fh, '<', 'bio.fasta' or die "open error: $!";

   # 'k' or 'm' indicates kibiBytes (KiB) or mebiBytes (MiB) respectively.
   # Read continues reading until reaching the record separator or EOF.
   # Optionally, one may specify the record separator.

   $/ = "\n>";

   while ( read($fh, my($buf), '2k') ) {
      print "# chunk number: $.\n";
      print "$buf\n";
   }

C<$.> contains the chunk_id above or the record_number below. C<readline($fh)>
or C<<$fh>> may be used for reading a single record.

   while ( my $buf = <$fh> ) {
      print "# record number: $.\n";
      print "$buf\n";
   }

The following provides a parallel demonstration. Workers receive the next chunk
from the shared-manager process where the actual read takes place. MCE::Shared
also works with C<threads>, C<forks>, and likely other parallel modules.

   use MCE::Hobo;       # (change to) use threads; (or) use forks;
   use MCE::Shared;
   use feature qw( say );

   my $pattern  = 'something';
   my $hugefile = 'somehuge.log';

   my $result = MCE::Shared->array();
   mce_open my $fh, "<", $hugefile or die "open error: $!";

   sub task {
      # the trailing 'k' or 'm' for size enables chunk IO
      while ( read $fh, my( $slurp_chunk ), "640k" ) {
         my $chunk_id = $.;
         # process chunk only if a match is found; ie. fast scan
         # optionally, comment out the if statement and closing brace
         if ( $slurp_chunk =~ /$pattern/m ) {
            my @matches;
            while ( $slurp_chunk =~ /([^\n]+\n)/mg ) {
               my $line = $1; # save $1 to not lose the value
               push @matches, $line if ( $line =~ /$pattern/ );
            }
            $result->push( @matches ) if @matches;
         }
      }
   }

   MCE::Hobo->create('task') for 1 .. 4;

   # do something else

   MCE::Hobo->waitall();

   say $result->len();

For comparison, the same thing using C<MCE::Flow>. MCE workers read the file
directly when given a plain path, so will have lesser overhead. However, the
run time is similar if one were to pass a file handle instead to mce_flow_f.

The benefit of chunk IO is from lesser IPC for the shared-manager process
(above). Likewise, for the mce-manager process (below).

   use MCE::Flow;
   use feature qw( say );

   my $pattern  = 'something';
   my $hugefile = 'somehuge.log';

   my @result = mce_flow_f {
      max_workers => 4, chunk_size => '640k',
      use_slurpio => 1,
   },
   sub {
      my ( $mce, $slurp_ref, $chunk_id ) = @_;
      # process chunk only if a match is found; ie. fast scan
      # optionally, comment out the if statement and closing brace
      if ( $$slurp_ref =~ /$pattern/m ) {
         my @matches;
         while ( $$slurp_ref =~ /([^\n]+\n)/mg ) {
            my $line = $1; # save $1 to not lose the value
            push @matches, $line if ( $line =~ /$pattern/ );
         }
         MCE->gather( @matches ) if @matches;
      }
   }, $hugefile;

   say scalar( @result );

=head1 LIMITATION

Perl must have L<IO::FDPass> for constructing a shared C<condvar>, C<handle>,
or C<queue> while the shared-manager process is running. For platforms where
C<IO::FDPass> is not feasible, construct any C<condvar>, C<handle>, and
C<queue> first before other classes. The shared-manager process is delayed
until sharing other classes or starting the manager explicitly.

   use MCE::Shared;

   my $cv  = MCE::Shared->condvar();
   my $que = MCE::Shared->queue();

   mce_open my $fh, ">>", "/path/to/file.log";

   MCE::Shared->start();

When passing a C<reference>, be sure to construct its C<file handle> associated
with C<reference> prior to the shared-manager process being spawned.

   mce_open my $fh, ">>", \*non_shared_fh;

=head1 CREDITS

Implementation inspired by L<Tie::StdHandle>.

=head1 INDEX

L<MCE|MCE>, L<MCE::Hobo>, L<MCE::Shared>

=head1 AUTHOR

Mario E. Roy, S<E<lt>marioeroy AT gmail DOT comE<gt>>

=cut


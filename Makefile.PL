
# Module makefile for MCE (using ExtUtils::MakeMaker)

use 5.010001;
use strict;
use warnings;

use ExtUtils::MakeMaker;

# Having IO::FDPass is beneficial for Condvar(s), Handle(s), and Queue(s).
# Therefore, add IO::FDPass to PREREQ_PM if we can and not already installed.
# Run MCE_PREREQ_EXCLUDE_IO_FDPASS=1 perl Makefile.PL to bypass the check.

my %require_io_fdpass;

if ( !$ENV{'MCE_PREREQ_EXCLUDE_IO_FDPASS'} ) {
   my $add_io_fdpass;

   # cygwin lacks file descriptor passing
   $add_io_fdpass = 0 if $^O =~ /mingw|msys|cygwin/i;

   # install IO::FDPass manually if desired
   $add_io_fdpass = 0 if $^O =~ /aix|hp-?ux|irix|gnu/i;
   $add_io_fdpass = 0 if $^O =~ /osf|solaris|sunos|svr5|sco/i;

   # okay on BSD variants, Linux, and Microsoft Windows
   $add_io_fdpass = 1 if $^O =~ /bsd|darwin|dragonfly|linux|mswin/i;

   if ( $add_io_fdpass ) {
      local $@; local $SIG{'__DIE__'};
      eval 'use IO::FDPass; 1' or $require_io_fdpass{'IO::FDPass'} = '1.1';
   }
}

# Make Makefile

WriteMakefile(
   ABSTRACT => 'MCE extension for sharing data supporting threads and processes',
   AUTHOR   => 'Mario E. Roy <marioeroy AT gmail DOT com>',
   NAME     => 'MCE::Shared',
   VERSION  => '1.808',

   NO_META  => 1,

   PREREQ_PM => {
      'bytes'        => 0,
      'constant'     => 0,
      'overload'     => 0,
      'strict'       => 0,
      'warnings'     => 0,
      'Carp'         => 0,
      'MCE'          => 1.809,
      'Scalar::Util' => 0,
      'Socket'       => 0,
      'Storable'     => 2.04,
      'Symbol'       => 0,
      'Time::HiRes'  => 0,
      %require_io_fdpass
   },

   ((!eval { ExtUtils::MakeMaker->VERSION(6.25) } ) ?
      ( PL_FILES => {} ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.31) } ) ?
      ( LICENSE => 'perl' ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.46) } ) ?
      ( META_MERGE => {
         'build_requires' => {
            'ExtUtils::MakeMaker' => 0,
            'Test::More'          => 0.88
         },
         'resources' => {
            'bugtracker' => 'https://github.com/marioroy/mce-shared/issues',
            'homepage'   => 'https://github.com/marioroy/mce-shared',
            'license'    => 'http://dev.perl.org/licenses/',
            'repository' => 'https://github.com/marioroy/mce-shared.git'
         },
         'provides' => {
            'MCE::Hobo' => {
               'file'    => 'lib/MCE/Hobo.pm',
               'version' => '1.808'
            },
            'MCE::Shared' => {
               'file'    => 'lib/MCE/Shared.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Array' => {
               'file'    => 'lib/MCE/Shared/Array.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Base' => {
               'file'    => 'lib/MCE/Shared/Base.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Condvar' => {
               'file'    => 'lib/MCE/Shared/Condvar.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Handle' => {
               'file'    => 'lib/MCE/Shared/Handle.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Hash' => {
               'file'    => 'lib/MCE/Shared/Hash.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Minidb' => {
               'file'    => 'lib/MCE/Shared/Minidb.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Ordhash' => {
               'file'    => 'lib/MCE/Shared/Ordhash.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Queue' => {
               'file'    => 'lib/MCE/Shared/Queue.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Scalar' => {
               'file'    => 'lib/MCE/Shared/Scalar.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Sequence' => {
               'file'    => 'lib/MCE/Shared/Sequence.pm',
               'version' => '1.808'
            },
            'MCE::Shared::Server' => {
               'file'    => 'lib/MCE/Shared/Server.pm',
               'version' => '1.808'
            }
         }
      } ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.48) } ) ?
      ( MIN_PERL_VERSION => 5.010001 ) : () ),

   INSTALLDIRS => ( ($] < 5.011) ? 'perl' : 'site' )
);


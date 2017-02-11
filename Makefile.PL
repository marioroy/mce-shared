
# Module makefile for MCE (using ExtUtils::MakeMaker)

use 5.010001;
use strict;
use warnings;

use ExtUtils::MakeMaker;

my $has_gcc = ( $^O eq 'MSWin32' )
   ? `gcc.exe --version 2>nul`
   : `gcc --version 2>/dev/null`;

# IO::FDPass is beneficial for MCE::Shared::Condvar, Handle, and Queue.
# Thus, add IO::FDPass to PREREQ_PM if we can and not already installed.

my %io_fdpass;

if ( ! $ENV{'MCE_PREREQ_EXCLUDE_IO_FDPASS'} && $has_gcc ) {
   local $@; local $SIG{'__DIE__'};
   if ( $^O =~ /mingw|msys|cygwin/i ) {
      # cygwin and friends lack file-descriptor passing capability
   }
   elsif ( $^O =~ /bsd|darwin|dragonfly|linux|mswin/i ) {
      eval 'use IO::FDPass; 1' or do {  ## no critic
         warn "\n",
            "## The IO::FDPass module is known to work on this platform.\n",
            "## Thus, adding IO::FDPass to PREREQ for a better experience\n",
            "## with MCE::Shared::Condvar, Handle, and Queue.\n\n";

         $io_fdpass{'IO::FDPass'} = '1.1';
      };
   }
   elsif ( $^O =~ /aix|hp-?ux|irix|gnu|osf|solaris|sunos|svr5|sco/i ) {
      eval 'use IO::FDPass; 1' or do {  ## no critic
         warn "\n",
            "## The IO::FDPass module is optional on this platform.\n",
            "## Install IO::FDPass manually for a better experience\n",
            "## with MCE::Shared::Condvar, Handle, and Queue.\n\n";
      };
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
      %io_fdpass
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
            'MCE::Shared::Cache' => {
               'file'    => 'lib/MCE/Shared/Cache.pm',
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
         },
         'prereqs' => {
            runtime => {
               recommends => {
                  'IO::FDPass' => 1.1,
                  'Sereal'     => 3.015
               }
            }
         }
      } ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.48) } ) ?
      ( MIN_PERL_VERSION => 5.010001 ) : () ),

   INSTALLDIRS => ( ($] < 5.011) ? 'perl' : 'site' )
);


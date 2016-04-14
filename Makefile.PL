
# Module makefile for MCE (using ExtUtils::MakeMaker)

use 5.010001;
use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
   ABSTRACT => 'MCE extension for sharing data supporting threads and processes',
   AUTHOR   => 'Mario E. Roy <marioeroy AT gmail DOT com>',
   NAME     => 'MCE::Shared',
   VERSION  => '1.002',

   PREREQ_PM => {
      'bytes'        => 0,
      'constant'     => 0,
      'overload'     => 0,
      'strict'       => 0,
      'warnings'     => 0,
      'Carp'         => 0,
      'MCE'          => 1.702,
      'POSIX'        => 0,
      'Scalar::Util' => 0,
      'Socket'       => 0,
      'Storable'     => 2.04,
      'Symbol'       => 0,
      'Time::HiRes'  => 0
   },

   ((!eval { ExtUtils::MakeMaker->VERSION(6.25) } ) ?
      ( PL_FILES => {} ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.31) } ) ?
      ( LICENSE => 'perl' ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.46) } ) ?
      ( META_MERGE => {
         'build_requires' => {
            'ExtUtils::MakeMaker' => 0,
            'Test::More'          => 0.45
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
               'version' => '1.002'
            },
            'MCE::Shared' => {
               'file'    => 'lib/MCE/Shared.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Array' => {
               'file'    => 'lib/MCE/Shared/Array.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Base' => {
               'file'    => 'lib/MCE/Shared/Base.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Condvar' => {
               'file'    => 'lib/MCE/Shared/Condvar.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Handle' => {
               'file'    => 'lib/MCE/Shared/Handle.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Hash' => {
               'file'    => 'lib/MCE/Shared/Hash.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Minidb' => {
               'file'    => 'lib/MCE/Shared/Minidb.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Ordhash' => {
               'file'    => 'lib/MCE/Shared/Ordhash.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Queue' => {
               'file'    => 'lib/MCE/Shared/Queue.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Scalar' => {
               'file'    => 'lib/MCE/Shared/Scalar.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Sequence' => {
               'file'    => 'lib/MCE/Shared/Sequence.pm',
               'version' => '1.002'
            },
            'MCE::Shared::Server' => {
               'file'    => 'lib/MCE/Shared/Server.pm',
               'version' => '1.002'
            }
         }
      } ) : () ),

   (( eval { ExtUtils::MakeMaker->VERSION(6.48) } ) ?
      ( MIN_PERL_VERSION => 5.010001 ) : () ),

   INSTALLDIRS => ( ($] < 5.011) ? 'perl' : 'site' )
);


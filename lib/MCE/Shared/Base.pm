###############################################################################
## ----------------------------------------------------------------------------
## Base package for helper classes.
##
###############################################################################

package MCE::Shared::Base;

use 5.010001;
use strict;
use warnings;

no warnings qw( threads recursion uninitialized numeric );

our $VERSION = '1.002';

################################################################################
#+----------------------------------------------------------------------------+#
#|                                     ##                                     |#
#|                                    #/\#                                    |#
#|                                   #//\\#                                   |#
#|                          #///////#///\\\#\\\\\\\#                          |#
#|                 #///P///#///E///#///  \\\#\\\R\\\#\\\L\\\#                 |#
#|        #///////#//// //#//// //#/////\\\\\#\\ \\\\#\\ \\\\#\\\\\\\#        |#
#|       #//// //#///////#///////#//////\\\\\\#\\\\\\\#\\\\\\\#\\ \\\\#       |#
#|        '==' => sub { $_[0] == $_[1] && looks_like_number ($_[0]) },        |#
#|        '!=' => sub { $_[0] != $_[1] && looks_like_number ($_[0]) },        |#
#|        '<'  => sub { $_[0] <  $_[1] && looks_like_number ($_[0]) },        |#
#|        '<=' => sub { $_[0] <= $_[1] && looks_like_number ($_[0]) },        |#
#|        '>'  => sub { $_[0] >  $_[1] && looks_like_number ($_[0]) },        |#
#|        '>=' => sub { $_[0] >= $_[1] && looks_like_number ($_[0]) },        |#
#|        'eq' => sub {              !ref ($_[0]) && $_[0] eq $_[1] },        |#
#|        'ne' => sub {  +--+  +--+  !ref ($_[0]) && $_[0] ne $_[1] },        |#
#|        'lt' => sub {  |  |  |  |  !ref ($_[0]) && $_[0] lt $_[1] },        |#
#|        'le' => sub {  +--+  +--+  !ref ($_[0]) && $_[0] le $_[1] },        |#
#|        'gt' => sub {              !ref ($_[0]) && $_[0] gt $_[1] },        |#
#|        'ge' => sub {         ++   !ref ($_[0]) && $_[0] ge $_[1] },        |#
#|        '=~' => sub {              !ref ($_[0]) && $_[0] =~ $_[1] },        |#
#|        '!~' => sub {              !ref ($_[0]) && $_[0] !~ $_[1] },        |#
#|            ####   /    Welcome;    \   ####   ####   ####   ####           |#
#|           ####   /                  \   ####   ####   ####   ####          |#
#|                 /                    \                                     |#
#|                                            Perl Palace, MR 01/2016         |#
#+----------------------------------------------------------------------------+#
################################################################################

## no critic (BuiltinFunctions::ProhibitStringyEval)
## do not remove numeric from no warnings above

use Scalar::Util qw( looks_like_number );
use bytes;

##  o Find feature
##
##  o Used by MCE::Shared::{ Array, Hash, Minidb, and Ordhash }
##    methods which take a query string for an argument.
##
##  o Basic demonstration: @keys = $oh->keys( "val =~ /pattern/" );
##  o Supported operators: =~ !~ eq ne lt le gt ge == != < <= > >=
##  o Multiple expressions delimited by :AND or :OR
##  o Quoting optional inside the string
##
##    "key eq 'some key' :or (val > 5 :and val < 9)"
##    "key eq some key :or (val > 5 :and val < 9)"
##    "key =~ /pattern/i :and field =~ /pattern/i"
##    "key =~ /pattern/i :and index =~ /pattern/i"
##    "key =~ /pattern/i :and field eq 'foo bar'"   # address eq 'foo bar'
##    "key =~ /pattern/i :and field eq foo bar"     # address eq foo bar
##    "index eq 'foo baz' :or key !~ /pattern/i"    # 9 eq 'foo baz'
##    "index eq foo baz :or key !~ /pattern/i"      # 9 eq foo baz
##
##    MCE::Shared::{ Array, Hash, Ordhash }
##    * key matches on keys in the hash or index in the array
##    * val matches on values
##
##    MCE::Shared::Minidb
##    * key   matches on primary keys in the hash (H)oH or (H)oA
##    * field matches on HoH->{key}{field} e.g. address
##    * index matches on HoA->{key}[index] e.g. 9
##
##  o The modifiers :AND and :OR may be mixed case. e.g. :And

sub _compile {
   my ( $query ) = @_;
   my ( $len, @p ) = ( 0 );

   $query =~ s/^[\t ]+//;            # strip white-space
   $query =~ s/[\t ]+$//;
   $query =~ s/\([\t ]+/(/g;
   $query =~ s/[\t ]+\)/)/g;

   for ( split( /[\t ]:(?:and|or)[\t ]/i, $query ) ) {
      $len += length;

      if ( /([\(]*)([^\(]+)[\t ]+(=~|!~)[\t ]+(.*)/ ) {
         push @p, "$1($2 $3 $4)"
      }
      elsif ( /([\(]*)([^\(]+)[\t ]+(==|!=|<|<=|>|>=)[\t ]+([^\)]+)(.*)/ ) {
         push @p, "$1($2 $3 q($4) && looks_like_number($2))$5";
      }
      elsif ( /([\(]*)([^\(]+)[\t ]+(eq|ne|lt|le|gt|ge)[\t ]+([^\)]+)(.*)/ ) {
         ( $4 eq 'undef' )
            ? push @p, "$1(!ref($2) && $2 $3 undef)$5"
            : push @p, "$1(!ref($2) && $2 $3 q($4))$5";
      }
      else {
         push @p, $_;
      }

      $len += 6, push @p, " && " if ( lc ( substr $query, $len, 3 ) eq " :a" );
      $len += 5, push @p, " || " if ( lc ( substr $query, $len, 3 ) eq " :o" );
   }

   $query = join('', @p);
   $query =~ s/q\([\'\"]([^\(\)]*)[\'\"]\)/q($1)/g;

   $query;
}

###############################################################################
## ----------------------------------------------------------------------------
## Find items in ARRAY. Called by MCE::Shared::Array.
##
###############################################################################

sub _find_array {
   my ( $data, $params, $query ) = @_;
   my $q = _compile( $query );

   # array key
   $q =~ s/key[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$_ $1/gi;
   $q =~ s/(looks_like_number)\(key\)/$1(\$_)/gi;
   $q =~ s/(!ref)\(key\)/$1(\$_)/gi;

   # array value
   $q =~ s/val[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$data->[\$_] $1/gi;
   $q =~ s/(looks_like_number)\(val\)/$1(\$data->[\$_])/gi;
   $q =~ s/(!ref)\(val\)/$1(\$data->[\$_])/gi;

   local $SIG{__WARN__} = sub {
      print {*STDERR} "\nfind error: $_[0]\n  query: $query\n  eval : $q\n";
   };

   # wants keys
   if ( $params->{'getkeys'} ) {
      eval qq{ map { ($q) ? (\$_) : () } 0 .. \$#{ \$data } };
   }

   # wants values
   elsif ( $params->{'getvals'} ) {
      eval qq{ map { ($q) ? (\$data->[\$_]) : () } 0 .. \$#{ \$data } };
   }

   # wants pairs
   else {
      eval qq{ map { ($q) ? (\$_ => \$data->[\$_]) : () } 0 .. \$#{ \$data } };
   }
}

###############################################################################
## ----------------------------------------------------------------------------
## Find items in HASH. Called by MCE::Shared::{ Hash, Minidb, Ordhash }.
##
###############################################################################

sub _find_hash {
   my ( $data, $params, $query, $obj ) = @_;
   my $q = _compile( $query );
   my $grepvals = 0;

   # hash key
   $q =~ s/key[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$_ $1/gi;
   $q =~ s/(looks_like_number)\(key\)/$1(\$_)/gi;
   $q =~ s/(!ref)\(key\)/$1(\$_)/gi;

   # Minidb (HoH) field
   if ( $params->{'hfind'} ) {
      $q =~ s/\$_ /:%: /g;  # preserve $_ from hash key mods above
      $q =~ s/([^:%\(\t ]+)[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$data->{\$_}{'$1'} $2/gi;
      $q =~ s/:%: /\$_ /g;  # restore hash key mods
      $q =~ s/(looks_like_number)\(([^\$\)]+)\)/$1(\$data->{\$_}{'$2'})/gi;
      $q =~ s/(!ref)\(([^\$\)]+)\)/$1(\$data->{\$_}{'$2'})/gi;
   }

   # Minidb (HoA) field
   elsif ( $params->{'lfind'} ) {
      $q =~ s/\$_ /:%: /g;  # preserve $_ from hash key mods above
      $q =~ s/([^:%\(\t ]+)[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$data->{\$_}['$1'] $2/gi;
      $q =~ s/:%: /\$_ /g;  # restore hash key mods
      $q =~ s/(looks_like_number)\(([^\$\)]+)\)/$1(\$data->{\$_}['$2'])/gi;
      $q =~ s/(!ref)\(([^\$\)]+)\)/$1(\$data->{\$_}['$2'])/gi;
   }

   # Hash/Ordhash value
   elsif ( $params->{'getvals'} && $q !~ /\(\$_/ ) {
      $grepvals = 1;
      $q =~ s/val[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$_ $1/gi;
      $q =~ s/(looks_like_number)\(val\)/$1(\$_)/gi;
      $q =~ s/(!ref)\(val\)/$1(\$_)/gi;
   }
   else {
      $q =~ s/val[ ]+(==|!=|<|<=|>|>=|eq|ne|lt|le|gt|ge|=~|!~)/\$data->{\$_} $1/gi;
      $q =~ s/(looks_like_number)\(val\)/$1(\$data->{\$_})/gi;
      $q =~ s/(!ref)\(val\)/$1(\$data->{\$_})/gi;
   }

   local $SIG{__WARN__} = sub {
      print {*STDERR} "\nfind error: $_[0]\n  query: $query\n  eval : $q\n";
   };

   # wants keys
   if ( $params->{'getkeys'} ) {
      eval qq{ map { ($q) ? (\$_) : () } \$obj->keys };
   }

   # wants values
   elsif ( $params->{'getvals'} ) {
      $grepvals
         ? eval qq{ grep { ($q) } \$obj->vals }
         : eval qq{  map { ($q) ? (\$data->{\$_}) : () } \$obj->keys };
   }

   # wants pairs
   else {
      eval qq{ map { ($q) ? (\$_ => \$data->{\$_}) : () } \$obj->keys };
   }
}

###############################################################################
## ----------------------------------------------------------------------------
## Miscellaneous.
##
###############################################################################

sub _stringify { no overloading;    "$_[0]" }
sub _numify    { no overloading; 0 + $_[0]  }

# Croak and die handler.

sub _croak {
   if (defined $MCE::VERSION) {
      goto &MCE::_croak;
   }
   else {
      require Carp unless $INC{'Carp.pm'};
      $SIG{__DIE__} = \&_die;
      local $\ = undef; goto &Carp::croak;
   }
}

sub _die {
   if (!defined $^S || $^S) {
      if ( ($INC{'threads.pm'} && threads->tid() != 0) ||
            $ENV{'PERL_IPERL_RUNNING'}
      ) {
         # thread env or running inside IPerl, check stack trace
         my $_t = Carp::longmess(); $_t =~ s/\teval [^\n]+\n$//;
         if ( $_t =~ /^(?:[^\n]+\n){1,7}\teval / ||
              $_t =~ /\n\teval [^\n]+\n\t(?:eval|Try)/ )
         {
            CORE::die(@_);
         }
      }
      else {
         # normal env, trust $^S
         CORE::die(@_);
      }
   }

   print {*STDERR} $_[0] if defined $_[0];

   ($^O eq 'MSWin32')
      ? CORE::kill('KILL', -$$, $$)
      : CORE::kill('INT', -getpgrp);

   CORE::exit($?);
}

1;

__END__

###############################################################################
## ----------------------------------------------------------------------------
## Module usage.
##
###############################################################################

=head1 NAME

MCE::Shared::Base - Base package for helper classes

=head1 VERSION

This document describes MCE::Shared::Base version 1.002

=head1 DESCRIPTION

Common functions for L<MCE::Shared>. There is no public API.

=head1 INDEX

L<MCE|MCE>, L<MCE::Hobo>, L<MCE::Shared>

=head1 AUTHOR

Mario E. Roy, S<E<lt>marioeroy AT gmail DOT comE<gt>>

=cut


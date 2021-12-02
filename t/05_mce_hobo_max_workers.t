#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;

BEGIN {
   use_ok 'MCE::Hobo';
}

{
   no warnings 'redefine';
   sub MCE::Util::get_ncpu { return 16; }
}

{
   # When max_workers is specified... (default undef, unlimited)
   # Going higher than the HW ncpu limit is possible. Simply specify the
   # number of workers desired. The minimum number of workers is 1.

   MCE::Hobo->init(max_workers => 0);
   is(MCE::Hobo->max_workers(), 1, "check that max_workers=>0 is 1");

   MCE::Hobo->max_workers(5);
   is(MCE::Hobo->max_workers(), 5, "check that max_workers=>5 is 5");

   MCE::Hobo->max_workers(20);
   is(MCE::Hobo->max_workers(), 20, "check that max_workers=>20 is 20");
}

{
   # 'auto' is the number of logical processors.

   MCE::Hobo->init(max_workers => 'auto');
   is(MCE::Hobo->max_workers(), 16,
      "check that max_workers=>'auto' is 16 logical cores"
   );
}

{
   # One may specify a percentage starting with MCE::Hobo 1.874.
   # The minimum number of workers is 1.

   MCE::Hobo->init(max_workers => '0%');
   is(MCE::Hobo->max_workers(), 1,
      "check that max_workers=>'0%' is 1 on HW with 16 logical cores"
   );

   MCE::Hobo->max_workers('1%');
   is(MCE::Hobo->max_workers(), 1,
      "check that max_workers=>'1%' is 1 on HW with 16 logical cores"
   );

   MCE::Hobo->max_workers('25%');
   is(MCE::Hobo->max_workers(), 4,
      "check that max_workers=>'25%' is 4 on HW with 16 logical cores"
   );

   MCE::Hobo->max_workers('37.5%');
   is(MCE::Hobo->max_workers(), 6,
      "check that max_workers=>'37.5%' is 6 on HW with 16 logical cores"
   );

   MCE::Hobo->max_workers('100%');
   is(MCE::Hobo->max_workers(), 16,
      "check that max_workers=>'100%' is 16 on HW with 16 logical cores"
   );

   MCE::Hobo->max_workers('200%');
   is(MCE::Hobo->max_workers(), 32,
      "check that max_workers=>'200%' is 32 on HW with 16 logical cores"
   );
}

done_testing;


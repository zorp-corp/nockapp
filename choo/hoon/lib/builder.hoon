/*  hoon-139-hoon  %hoon  /lib/hoon-139/hoon
/*  wrapper-hoon  %hoon  /lib/wrapper/hoon
/*  kernel-hoon  %hoon  /lib/kernel/hoon
!.
::
::  Bootstrap builder: to build the bootstrap formula for Choo using
::  Urbit Ford
::
::  sync files into a desk %choo
::  dojo> =choo -build-file /=choo=/lib/builder/hoon
::  dojo> .choo/jam choo
::
::  copy <your-fakezod>/.urb/put/choo.jam to choo/bootstrap/choo.jam
^-  *
~&  "compiling hoon"
=/  hoon-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut %noun) %noun (ream hoon-139-hoon))
~&  "compiling wrapper"
=/  wrapper-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut t.hoon-knob) %noun (ream wrapper-hoon))
~&  "compiling kernel"
=/  kernel-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut t.wrapper-knob) %noun (rain /lib/choo/kernel/hoon kernel-hoon))
=/  trap-nock=nock
  [%7 [%7 form.hoon-knob form.wrapper-knob] form.kernel-knob]
~&  %built-trap-nock
trap-nock
::  TODO: use this once we can execute trap in NockApp
::=>  [trap=trap-nock hash=(mug trap-nock)]
::|.  .*([.*(0 trap) hash] [%9 2 %10 [6 %0 3] %0 2])

/*  hoon-139-hoon  %hoon  /lib/hoon-139/hoon
/*  kernel-hoon  %hoon  /lib/http-kernel/hoon
!.
::
::  Bootstrap builder: to build the bootstrap formula for Choo using
::  Urbit Ford
::
::  sync files into a desk %choo
::  dojo> =http-ker -build-file /=choo=/lib/builder/hoon
::  dojo> .kernel-/jam http-ker
::
::  copy <your-fakezod>/.urb/put/kernel.jam to http-app/bootstrap/kernel.jam
^-  nock
~&  "compiling hoon"
=/  hoon-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut %noun) %noun (ream hoon-139-hoon))
~&  "compiling kernel"
=/  kernel-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut t.hoon-knob) %noun (rain /lib/http-kernel/hoon kernel-hoon))
[%7 form.hoon-knob form.kernel-knob]

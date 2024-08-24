/*  hoon-139-hoon  %hoon  /lib/hoon-139/hoon
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
^-  nock
~&  "compiling hoon"
=/  hoon-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut %noun) %noun (ream hoon-139-hoon))
~&  "compiling kernel"
=/  kernel-knob=[t=type form=nock]
  ~>  %bout
  (~(mint ut t.hoon-knob) %noun (rain /lib/choo/kernel/hoon kernel-hoon))
[%7 form.hoon-knob form.kernel-knob]

/*  hoon-139-hoon  %hoon  /lib/hoon-139/hoon
/*  wrapper-hoon  %hoon  /lib/wrapper/hoon
/*  kernel-hoon  %hoon  /lib/http-kernel/hoon
!.
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
  (~(mint ut t.wrapper-knob) %noun (rain /lib/http-app/http-kernel/hoon kernel-hoon))
=/  trap-nock=nock
  [%7 [%7 form.hoon-knob form.wrapper-knob] form.kernel-knob]
~&  %built-trap-nock
trap-nock
::  TODO: use this once we can execute trap in NockApp
::=>  [trap=trap-nock hash=(mug trap-nock)]
::|.  .*([.*(0 trap) hash] [%9 2 %10 [6 %0 3] %0 2])


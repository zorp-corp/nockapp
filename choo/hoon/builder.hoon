/*  hoon-139-hoon  %hoon  /lib/hoon-139/hoon
/*  kernel-hoon  %hoon  /lib/kernel/hoon
!.
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

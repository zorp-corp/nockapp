/+  *wrapper
=>
|%
+$  test-state  [%0 val=@]
++  moat  (keep test-state)
+$  cause  ?(%inc %inc-exit)
::
+$  effect
  $%  [%state test-state]
      [%exit id=@]
  ==
--
::
%-  (moat |)
^-  fort:moat
|_  k=test-state
::
::  +load: upgrade from previous state
::
++  load
  |=  arg=test-state
  ~&  >>  "load"
  arg
::
::  +peek: external inspect
::
++  peek
  |=  arg=*
  ^-  (unit (unit *))
  =/  pax  ((soft path) arg)
  ?~  pax  ~|(invalid-peek+pax !!)
  ~&  >  "peeked at {<u.pax>}"
  ?+  u.pax  ~|(invalid-peek+pax !!)
  ::
      [%state ~]
    ``k
  ==
::
::  +poke: external apply
::
++  poke
  |=  [=wire eny=@ our=@ux now=@da dat=*]
  ^-  [(list effect) test-state]
  ~&  >>  "poke: {<wire>}"
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    ~&  dat
    !!
  ?+    `@tas`u.sof-cau  !!
      %inc
    =.  val.k  +(val.k)
    :_  k
    =-  ~&  effect+-
      -
    ~[[%state k]]
  ::
     %inc-exit
    =.  val.k  +(val.k)
    :_  k
    =-  ~&  effect+-
      -
    ~[[%exit 0] [%state k]]
  ==
--

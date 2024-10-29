/+  *wrapper
=>
|%
+$  server-state  @
++  moat  (keep server-state)
+$  cause  %inc
::
+$  effect
  $:  %state
      val=@
  ==
--
::
%-  moat
^-  fort:moat
|_  k=server-state
::
::  +load: upgrade from previous state
::
++  load
  |=  arg=*
  ^-  [(list *) *]
  !!
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
  |=  [eny=@ our=@ux now=@da dat=*]
  ^-  [(list effect) server-state]
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    ~&  dat
    !!
  =.  k  +(k)
  :_  k
  :_  ~
  ^-  effect
  =-  ~&  effect+-
      -
  ?>  ?=(%inc u.sof-cau)
  [%state val=k]
--

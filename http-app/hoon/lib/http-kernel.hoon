::/+  *wrapper
~&  %outermost
=>
|%
+$  server-state  %stateless
++  moat  (keep server-state)
+$  cause
  $:  %msg
      msg=@t
  ==
+$  effect  [%msg @t]
--
::
~&  %serving
=<  $
~&  %inside-buc-serving
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
  |=  path=*
  ^-  (unit (unit *))
  !!
::
::  +poke: external apply
::
++  poke
  ~&  %poking
  |=  [eny=@ our=@ux now=@da dat=*]
  ^-  [(list effect) server-state]
  ~&  dat+dat
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    !!
  =/  =cause  u.sof-cau
  ~&  cause+cause
  :_  k
  [%msg msg.cause]~
--


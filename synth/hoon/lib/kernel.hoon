/+  *wrapper
=>
|%
+$  state  %stateless
++  moat  (keep state)
+$  cause
  $:  %req
      ~
  ==
::
+$  effect
  $:  %res
      ~
  ==
::
++  to-octs
  |=  bod=@
  ^-  (unit octs)
  =/  len  (met 3 bod)
  ?:  =(len 0)  ~
  `[len bod]
--
::
~&  %synthing
%-  moat
^-  fort:moat
|_  k=state
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
  |=  =path
  ^-  (unit (unit *))
  !!
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
  [~ k]
--


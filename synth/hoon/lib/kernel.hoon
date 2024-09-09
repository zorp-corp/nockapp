/+  *wrapper
=>
|%
+$  state  %stateless
++  moat  (keep state)
+$  cause
  $:  %input
      p=cord
  ==
::
+$  effect
  $:  %emit
      num=@
  ==
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
  ^-  [(list effect) state]
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    ~&  dat
    !!
  =/  =cause  u.sof-cau
  =/  res  (slap !>(~) (ream p.cause))
  =/  out-atom  ;;(@ q.res)
  :_  k
  :_  ~
  [%emit out-atom]
--


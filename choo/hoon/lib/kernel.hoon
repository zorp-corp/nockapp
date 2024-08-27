::/+  *wrapper
=>
|%
+$  choo-state  %stateless
++  moat  (keep choo-state)
::
::  nock compiled from hoon, along with type. form is independent of
::  subject, usually expected to be 0
+$  knob  [t=type f=nock]
+$  cause  [%compile sub=knob pax=path fil=cord nob=?]
+$  effect  [%jam p=*]
--
::
~&  %choo-choo
%-  moat
^-  fort:moat
|_  k=choo-state
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
  |=  [event-num=@ eny=@ our=@ux now=@da dat=*]
  ^-  [(list *) choo-state]
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    !!
  =/  =cause  u.sof-cau
  |^
  =;  p=*
    ~&  nob+nob.cause
    [[%jam p]~ k]
  ?:  nob.cause
    (compile-knob [sub pax fil]:cause)
  (compile-nock [sub pax fil]:cause)
  ::
  ++  compile-knob
    |=   [sub=knob pax=path fil=cord]
    ~&  %compile-knob
    ^-  knob
    =/  ast
      ~&  %parsing  (rain pax fil)
    =/  [t=type form=nock]
      ~&  %compiling  (~(mint ut t.sub) %noun ast)
    [t %7 f.sub form]
  ::
  ++  compile-nock
    |=   [sub=knob pax=path fil=cord]
    ~&  %compile-knob
    ^-  nock
    =/  ast
      ~&  %parsing  (rain pax fil)
    =/  [t=type form=nock]
      ~&  %compiling  (~(mint ut t.sub) %noun ast)
    [%7 f.sub form]
  --
--

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
  =-  [[%jam -]~ k]
  (compile [sub pax fil nob]:cause)
  ::
  ++  compile
    |=   [sub=knob pax=path fil=cord nob=?]
    =/  ast
      ~&  %parsing  (rain pax fil)
    =/  [t=type form=nock]
      ~&  %compiling  (~(mint ut t.sub) %noun ast)
    ?:  nob
      ~&  %compiled-knob
      [t %7 f.sub form]
    ~&  %compiled-nock
    [%7 f.sub form]
  --
--

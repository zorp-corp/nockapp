::
::  arvo-shaped outer core
::
!:
=<
~&  "choo choo"
|_  k=kernel-state
+*  this  .
::
::  +load: upgrade from previous state
::
++  load
  ^-  ^
  !!
::  TODO work in ares
::  ++  load                                                :: 4
::    |=  hir=kernel-state
::    ^-  _this
::    ~&  >>>  hir+hir
::    =;  t  ~&  >>  k.t  t
::    =.  epoch.hir  +(epoch.k)
::    =.  chain.k  chain.hir
::    this(k hir)
::
::  +peek: external inspect
::
++  peek                                                :: 22
  |=  arg=*
  ^-  (unit (unit *))
  (do-peek k arg)
::
::  +poke: external apply
::
++  poke                                                :: 23
  |=  arg=*
  ^-  ^
  ::  ~&  "poked with arg {<arg>}"
  =^  effects  k  (do-poke k arg)
  [effects this(k k)]
::
::  +wish: external compute
::
++  wish                                                :: 10
  |=  txt=@
  !!
--
::
::  inner core that does the actual work
|%
+$  kernel-state  %stateless
::
::  nock compiled from hoon, along with type. form is independent of
::  subject, usually expected to be 0
+$  knob  [t=type f=nock]
+$  cause  [%compile sub=knob pax=path fil=cord nob=?]
+$  effect  [%jam p=*]
+$  goof  [mote=term =tang]
+$  ovum  $%([[%poke ~] =pok] [%crud =goof %poke =pok])
+$  pok   [eny=@ our=@ux now=@da =cause]
+$  crud  [=goof =pok]
++  do-poke
  |=  [k=kernel-state arg=*]
  ^-  [(list effect) kernel-state]
  |^
  ?@  arg  ~&("bad poke type: {<arg>}" !!)
  ?+    +.arg  ~&("invalid arg: {<arg>}" !!)
      [[%$ %arvo ~] %crud gof=* cru=*]
    =/  g  ((soft goof) gof.arg)
    ?~  g  ~&(%invalid-goof !!)
    (handle-crud u.g cru.arg)
  ::
      [[%poke ~] pok=*]
    =/  o  ((soft ovum) +.arg)
    ?~  o 
     ~&  "could not mold poke type: {<+.arg>}"
     =+  (road |.(;;(ovum +.arg)))
     ~^k
    ?.  ?=([%poke ~] -.u.o)
      ~&  %bad-poke-header  !!
    (handle-poke *@da pok.u.o)
  ==
  ::
  ++  handle-crud
    |=  [=goof cru=*]
    ^-  [(list effect) kernel-state]
    ~&  "%crud: event failed"
    =-  [~ k]
    %+  turn  tang.goof
    |=(g=tank ~>(%slog.[3 g] 0))
  ::
  ++  handle-poke
    |=  [event-num=@da eny=@ our=@ux now=@da =cause]
    ^-  [(list effect) kernel-state]
    :: ~&  "poked at {<now>}"
    =;  p=*
      ~&  nob+nob.cause
      [[%jam p]~ k]
    ?:  nob.cause
      (compile-knob [sub pax fil]:cause)
    (compile-nock [sub pax fil]:cause)
  --
++  compile-knob
  |=   [sub=knob pax=path fil=cord]
  ~&  %compile-knob
  ^-  knob
  =/  ast
    ~&  %parsing  (rain pax fil)
  =/  [t=type form=nock]
    ~&  %compiling  (~(mint ut t.sub) %noun ast)
  [t %7 f.sub form]
++  compile-nock
  |=   [sub=knob pax=path fil=cord]
  ~&  %compile-knob
  ^-  nock
  =/  ast
    ~&  %parsing  (rain pax fil)
  =/  [t=type form=nock]
    ~&  %compiling  (~(mint ut t.sub) %noun ast)
  [%7 f.sub form]
::
++  do-peek
  |=  [k=kernel-state arg=*]
  ^-  (unit (unit *))
  =/  path  ((soft path) arg)
  ~&(invalid-peek+path !!)
--

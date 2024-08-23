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
::
::  +peek: external inspect
::
++  peek                                                :: 22
  |=  arg=*
  ^-  (unit (unit *))
  !!
::
::  +poke: external apply
::
++  poke                                                :: 23
  |=  arg=*
  ^-  ^
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
+$  cause
  $%  [%compile sub=knob pax=path fil=cord nok=?]
      [%execute sub=knob pax=path fil=cord nok=? hoon=cord]
  ==
::
+$  effect  [%jam p=*]
+$  goof  [mote=term =tang]
+$  ovum  $%([[%poke ~] =pok] [%crud =goof %poke =pok])
+$  pok   [eny=@ our=@ux now=@da =cause]
+$  crud  [=goof =pok]
::
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
    =;  p=*
      [[%jam p]~ k]
    ?-    -.cause
        %compile
      =-  ~&  raw-nock+nok.cause
          -
      (compile [sub pax fil nok]:cause)
    ::
        %execute
      (execute [sub pax fil nok hoon]:cause)
    ==
  --
::
++  compile
  |=  [sub=knob pax=path fil=cord nok=?]
  ^-  *
  =/  ast
    ~&  %parsing  (rain pax fil)
  =/  [t=type form=nock]
    ~&  compiling+pax
    (~(mint ut t.sub) %noun ast)
  ?:  nok
    `nock`[%7 f.sub form]
  `knob`[t %7 f.sub form]
::
++  execute
  |=  [sub=knob pax=path fil=cord nok=? hoon=cord]
  ^-  *
  =/  ast
    ~&  parsing+pax
    (rain pax fil)
  =/  vax
    ~&  compiling-and-executing+pax
    (slap sub ast)
  =/  par
    ~&  %parsing-expression
    (ream hoon)
  =/  compiled
    ~&  %compiling-expression
    (~(mint ut p.vax) %noun par)
  ~&  %executing
  =/  out  .*(q.vax q.compiled)
  ~&  out
  ?:  nok
    out
  `vase`[p.compiled out]
--

|=  hash=@uvI
=<  .(desk-hash.a.k `hash)
::
::  arvo-shaped outer core
::
=<
|_  k=kernel-state
+*  this  .
::
::  +load: upgrade from previous state
::
++  load                                                ::  4
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
::
::  inner core that does the actual work
|%
+$  kernel-state
  $:  %0
      a=[desk-hash=(unit @uvI) ~]
  ==
::
+$  goof    [mote=term =tang]
+$  ovum
  $%  [[%poke ~] =pok]                                 ::  internal poke
      [%crud =goof %poke =pok]                         ::  forwarded error
  ==
::  $crud: kernel error wrapper
::
+$  crud    [=goof =pok]
::  $pok: kernel poke type
::
+$  pok     [eny=@ our=@ux now=@da =request]
::
+$  request
  $:  %serve-message
      uri=@t
      ~
  ==
::
++  do-poke
  |=  [k=kernel-state arg=*]
  ^-  [(list *) kernel-state]
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
    ?>  ?=([%poke ~] -.u.o)
    (handle-poke *@da pok.u.o)
  ==
  ::
  ++  handle-crud
    |=  [=goof cru=*]
    ^-  [(list *) kernel-state]
    ~&  "%crud: event failed"
    =-  [~ k]
    %+  turn  tang.goof
    |=(g=tank ~>(%slog.[3 g] 0))
  ::
  ++  handle-poke
    |=  [event-num=@da eny=@ our=@ux now=@da req=request]
    ^-  [(list *) kernel-state]
    ~&  "poked with {<req>}"
    :_  k
    [uri.req ~]
  --
--

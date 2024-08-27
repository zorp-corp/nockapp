|%
+$  goof    [mote=term =tang]
+$  ovum
  $%  [[%poke ~] =input]
      [%crud =goof %poke =input]
  ==
+$  crud    [=goof =input]
+$  input   [event-num=@ eny=@ our=@ux now=@da cause=*]
::
++  keep
  |*  inner=mold
  =>
  |%
  +$  inner-state  inner
  +$  outer-state  [desk-hash=(unit @uvI) internal=inner]
  +$  outer-fort
    $_  ^|
    |_  outer-state
    ++  load
      |~  arg=*
      *[(list *) *]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  arg=input
      *[(list *) outer-state]
    ++  wish
      |~  txt=@
      **
    --
  ::
  +$  fort
    $_  ^|
    |_  inner-state
    ++  load
      |~  arg=*
      *[(list *) *]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  arg=input
      *[(list *) inner-state]
    --
  --
  ::
  |=  inner=fort
  |=  hash=@uvI
  =<  .(desk-hash.outer `hash)
  |_  outer=outer-state
  ++  load
    |=  arg=*
    ^-  [(list *) *]
    (load:inner arg)
  ::
  ++  peek
    |=  arg=path
    ^-  (unit (unit *))
    (peek:inner arg)
  ::
  ++  wish
    |=  txt=@
    ^-  *
    q:(slap !>(~) (ream txt))
  ::
  ++  poke
    |=  =input
    ^-  [(list *) outer-state]
    ?@  input  ~&("bad poke type: {<input>}" !!)
    ?+    +.input  ~&("invalid arg: {<input>}" !!)
        [[%$ %arvo ~] %crud gof=* cru=*]
      =/  g  ((soft goof) gof.arg)
      ?~  g  ~&(%invalid-goof !!)
      =-  [~ outer]
      %+  turn  tang.u.g
      |=(=tank ~>(%slog.[3 tank] 0))
    ::
        [[%poke ~] pok=*]
      =/  o  ((soft ovum) +.input)
      ?~  o
        ~&  "could not mold poke type: {<+.input>}"
        =+  (road |.(;;(ovum +.input)))
        ~^outer
      ~&  "going to poke inner"
      ?>  ?=([%poke ~] -.u.o)
      =^  effects  internal.outer
        (poke:inner input)
      [effects outer]
    ==
  --
--

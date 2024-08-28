|%
+$  goof    [mote=term =tang]
+$  ovum
  $%  [[%poke ~] =input]
      [%crud =goof %poke =input]
  ==
+$  crud    [=goof =input]
+$  input   [eny=@ our=@ux now=@da cause=*]
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
      |~  [event-num=@ ovum]
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
    |=  [num=@ =ovum]
    ^-  [(list *) outer-state]
    ?+    ovum  ~&("invalid ovum: {<ovum>}" !!)
        [[%$ %arvo ~] *]
      =/  g  ((soft goof) gof.ovum)
      ?~  g  ~&(%invalid-goof !!)
      =-  [~ outer]
      %+  turn  tang.u.g
      |=(=tank ~>(%slog.[3 tank] 0))
    ::
        [[%poke ~] *]
      =/  o  ((soft input) input.ovum)
      ?~  o
        ~&  "could not mold poke type: {<ovum>}"
        =+  (road |.(;;(^ovum ovum)))
        ~^outer
      =^  effects  internal.outer
        (poke:inner input.ovum)
      [effects outer]
    ==
  --
--

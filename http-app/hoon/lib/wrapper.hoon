|%
+$  goof    [mote=term =tang]
+$  wire    path
+$  ovum    [=wire =input]
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
      [*(list *) **]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |*  [num=@ ovum=*]
      [*(list *) *outer-state]
    ++  wish
      |~  txt=@
      **
    --
  ::
  +$  fort
    $_  ^|
    |_  state=inner-state
    ++  load
      |~  arg=*
      [*(list *) **]
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  arg=input
      [*(list *) *inner-state]
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
    |=  txt=@t
    ^-  *
    (slap !>(~) (ream txt))
  ::
  ++  poke
    ~&  %poke-gate
    |=  [num=@ ovum=*]
    ^-  [(list *) _..poke]
    ~&  %poking
    ?+   ovum  ~&("invalid arg: {<ovum>}" ~^..poke)
        [[%$ %arvo ~] *]
      =/  g  ((soft crud) ovum)
      ?~  g  ~&(%invalid-goof ~^..poke)
      =-  [~ ..poke]
      %+  turn  tang.goof.u.g
      ~>  %slog.[3 leaf+"crud"]
      |=(=tank ~>(%slog.[3 tank] 0))
    ::
        [[%poke ~] *]
      =/  ovum  ((soft ^ovum) ovum)
      ?~  ovum  ~&("invalid arg: {<ovum>}" ~^..poke)
      =/  o  ((soft input) input.u.ovum)
      ?~  o
        ~&  "could not mold poke type: {<ovum>}"
        =+  (road |.(;;(^^ovum ovum)))
        ~^..poke
      =^  effects  internal.outer
        (poke:inner input.u.ovum)
      =.  state.inner  internal.outer
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
--

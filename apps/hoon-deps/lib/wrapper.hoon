|%
+$  goof    [mote=term =tang]
+$  wire    path
+$  ovum    [=wire =input]
+$  crud    [=goof =input]
+$  input   [eny=@ our=@ux now=@da cause=*]
::
++  keep
  |*  [inner=mold versioned=mold]
  =>
  |%
  +$  inner-state  inner
  +$  load-state
    $%  [%0 desk-hash=(unit @uvI) internal=versioned]
    ==
  +$  outer-state
    $%  [%0 desk-hash=(unit @uvI) internal=inner]
    ==
  +$  outer-fort
    $_  ^|
    |_  outer-state
    ++  load
      |~  arg=load-state
      **
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  [num=@ ovum=*]
      *[(list *) *]
    ++  wish
      |~  txt=@
      **
    --
  ::
  +$  fort
    $_  ^|
    |_  state=inner-state
    ++  load
      |~  arg=versioned
      *inner-state
    ++  peek
      |~  arg=path
      *(unit (unit *))
    ++  poke
      |~  arg=ovum
      [*(list *) *inner-state]
    --
  --
  ::
  |=  crash=?
  |=  inner=fort
  |=  hash=@uvI
  =<  .(desk-hash.outer `hash)
  |_  outer=outer-state
  +*  inner-fort  ~(. inner internal.outer)
  ++  load
    |=  arg=*
    =/  arg  ((soft load-state) arg)
    ?~  arg
      ~&  >>>  "+load: failed to soft state"  !!
    =/  new-internal  (load:inner-fort internal.u.arg)
    ..load(internal.outer new-internal)
  ::
  ++  peek
    |=  arg=path
    ^-  (unit (unit *))
    (peek:inner-fort arg)
  ::
  ++  wish
    |=  txt=@
    ^-  *
    q:(slap !>(~) (ream txt))
  ::
  ++  poke
    |=  [num=@ ovum=*]
    ^-  [(list *) _..poke]
    =/  effects=(list *)  ?:(crash ~[exit/0] ~)
    ?+   ovum  ~&("+poke: invalid ovum: {<ovum>}" effects^..poke)
        [[%$ %arvo ~] *]
      =/  g  ((soft crud) +.ovum)
      ?~  g  ~&("+poke: invalid-goof" effects^..poke)
      =-  [effects ..poke]
      (slog tang.goof.u.g)
    ::
        [[%poke *] *]
      =/  ovum  ((soft ^ovum) ovum)
      ?~  ovum  ~&("+poke: invalid ovum: {<ovum>}" ~^..poke)
      =/  o  ((soft input) input.u.ovum)
      ?~  o
        ~&  "+poke: could not mold poke type: {<o>}"
        ~^..poke
      =^  effects  internal.outer
        (poke:inner-fort u.ovum)
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
--

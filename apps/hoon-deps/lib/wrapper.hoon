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
  +$  outer-state
    $%  [%0 desk-hash=(unit @uvI) internal=inner]
    ==
  +$  outer-fort
    $_  ^|
    |_  outer-state
    ++  load
      |~  arg=*
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
      |~  arg=inner-state
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
    =/  shell=(unit outer-state)  ((soft outer-state) arg)
    ?~  shell
      ~&("+load: could not mold outer state {<arg>}" ~^..load)
    =/  new-internal  (load:inner-fort internal.u.shell)
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
        =+  (road |.(;;(^^ovum ovum)))
        ~^..poke
      =^  effects  internal.outer
        (poke:inner-fort u.ovum)
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
--

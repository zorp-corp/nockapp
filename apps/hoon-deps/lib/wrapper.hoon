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
      |~  arg=outer-state
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
    |=  old=outer-state
    ?+    -.old  ~&("+load: invalid old state" !!)
        %0
      =/  new-internal  (load:inner-fort internal.old)
      ..load(internal.outer new-internal)
    ==
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
    ?+   ovum  ~&("invalid arg: {<ovum>}" effects^..poke)
        [[%$ %arvo ~] *]
      =/  g  ((soft crud) +.ovum)
      ?~  g  ~&(%invalid-goof effects^..poke)
      =-  [effects ..poke]
      (slog tang.goof.u.g)
    ::
        [[%poke *] *]
      =/  ovum  ((soft ^ovum) ovum)
      ?~  ovum  ~&("invalid arg: {<ovum>}" ~^..poke)
      =/  o  ((soft input) input.u.ovum)
      ?~  o
        ~&  "could not mold poke type: {<ovum>}"
        =+  (road |.(;;(^^ovum ovum)))
        ~^..poke
      =^  effects  internal.outer
        (poke:inner-fort u.ovum)
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
--

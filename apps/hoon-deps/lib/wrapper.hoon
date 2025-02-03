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
    |_  state=inner
    ++  load
      |~  arg=inner
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
    =/  ovo  ((soft ^ovum) ovum)
    ?~  ovo
      ~&  "+poke: invalid ovum: {<ovum>}"
      ::
      ::  +road allows us to thread the clam failure into the stack trace
      =+  (road |.(;;(^ovum ovo)))
      ~^..poke
    ?+   u.ovo  ~&("+poke: invalid ovum: {<u.ovo>}" effects^..poke)
        [[%$ %arvo ~] *]
      =/  g  ((soft crud) +.u.ovo)
      ?~  g  ~&("+poke: invalid-goof" effects^..poke)
      =-  [effects ..poke]
      (slog tang.goof.u.g)
    ::
        [[%poke *] *]
      =/  o  ((soft input) input.u.ovo)
      ?~  o
        ~&  "+poke: could not mold input: {<input.u.ovo>}"
        =+  (road |.(;;(input input.u.ovo)))
        ~^..poke
      =^  effects  internal.outer
        (poke:inner-fort u.ovo)
      [effects ..poke(internal.outer internal.outer)]
    ==
  --
--

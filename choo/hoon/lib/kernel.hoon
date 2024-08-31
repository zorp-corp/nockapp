/+  *wrapper
/*  hoon-139-txt  %txt  /lib/hoon-139/hoon
=>
|%
+$  choo-state
  $:  cached-hoon=(unit (trap vase))
      ~
  ==
++  moat  (keep choo-state)
::
::  nock compiled from hoon, along with type. form is independent of
::  subject, usually expected to be 0
+$  knob  [t=type f=nock]
+$  cause
  $%  [%build entry=path nob=? directory=(map path cord)]
  ==
+$  effect  [%jam p=*]
--
::
=<  $
=<
~&  %choo-choo
%-  moat
^-  fort:moat
|_  k=choo-state
+*  builder  +>
::
::  +load: upgrade from previous state
::
++  load
  |=  arg=*
  ^-  [(list *) *]
  !!
::
::  +peek: external inspect
::
++  peek
  |=  path=*
  ^-  (unit (unit *))
  !!
::
::  +poke: external apply
::
++  poke
  |=  [eny=@ our=@ux now=@da dat=*]
  ^-  [(list *) choo-state]
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    !!
  =/  =cause  u.sof-cau
  =?  cached-hoon.k  ?=(~ cached-hoon.k)  `build-honc
  ?>  ?=(^ cached-hoon.k)
  :_  k
  :_  ~
  :-  %jam
  %-  ~(create builder u.cached-hoon.k)
  [entry directory]:cause
--
::
::  build system
::
=>
::
::  dependency system
::
|%
+$  raut
  ::  resolved taut - pax contains real path to file after running taut through +get-fit
  [face=(unit @tas) pax=path]
++  rile
  ::  resolved pile
  $:  sur=(list raut)
      lib=(list raut)
      raw=(list raut)
      bar=(list raut)
      =hoon
  ==
::
++  parse-pile
  |=  [pax=path tex=tape]
  ^-  pile
  =/  [=hair res=(unit [=pile =nail])]
    %-  road  |.
    ((pile-rule pax) [1 1] tex)
  ?^  res  pile.u.res
  %-  mean
  =/  lyn  p.hair
  =/  col  q.hair
  ^-  (list tank)
  :~  leaf+"syntax error at [{<lyn>} {<col>}] in {<pax>}"
    ::
      =/  =wain  (to-wain:format (crip tex))
      ?:  (gth lyn (lent wain))
        '<<end of file>>'
      (snag (dec lyn) wain)
    ::
      leaf+(runt [(dec col) '-'] "^")
  ==
::
++  pile-rule
  |=  pax=path
  %-  full
  %+  ifix
    :_  gay
    ::  parse optional /? and ignore
    ::
    ;~(plug gay (punt ;~(plug fas wut gap dem gap)))
  |^
  ;~  plug
    %+  cook  (bake zing (list (list taut)))
    %+  rune  hep
    (most ;~(plug com gaw) taut-rule)
  ::
    %+  cook  (bake zing (list (list taut)))
    %+  rune  lus
    (most ;~(plug com gaw) taut-rule)
  ::
    %+  rune  tis
    ;~(plug sym ;~(pfix gap stap))
  ::
    %+  rune  tar
    ;~  (glue gap)
      sym
      ;~(pfix cen sym)
      ;~(pfix stap)
    ==
  ::
    %+  stag  %tssg
    (most gap tall:(vang & pax))
  ==
  ::
  ++  pant
    |*  fel=rule
    ;~(pose fel (easy ~))
  ::
  ++  mast
    |*  [bus=rule fel=rule]
    ;~(sfix (more bus fel) bus)
  ::
  ++  rune
    |*  [bus=rule fel=rule]
    %-  pant
    %+  mast  gap
    ;~(pfix fas bus gap fel)
  --
::
++  taut-rule
  %+  cook  |=(taut +<)
  ;~  pose
    (stag ~ ;~(pfix tar sym))               ::  *foo -> [~ %foo]
    ;~(plug (stag ~ sym) ;~(pfix tis sym))  ::  bar=foo -> [[~ %bar] %foo]
    (cook |=(a=term [`a a]) sym)            ::  foo    -> [[~ %foo] %foo]
  ==
::
::  $taut: file import from /lib or /sur
::
+$  taut  [face=(unit term) pax=term]
++  segments
  |=  suffix=@tas
  ^-  (list path)
  =/  parser
    (most hep (cook crip ;~(plug ;~(pose low nud) (star ;~(pose low nud)))))
  =/  torn=(list @tas)  (fall (rush suffix parser) ~[suffix])
  %-  flop
  |-  ^-  (list (list @tas))
  ?<  ?=(~ torn)
  ?:  ?=([@ ~] torn)
    ~[torn]
  %-  zing
  %+  turn  $(torn t.torn)
  |=  s=(list @tas)
  ^-  (list (list @tas))
  ?>  ?=(^ s)
  ~[[i.torn s] [(crip "{(trip i.torn)}-{(trip i.s)}") t.s]]
++  get-fit
  |=  [pre=@ta pax=@tas dir=(map path cord)]
  ^-  (unit path)
  =/  paz=(list path)  (segments pax)
  |-
  ?~  paz  ~
  =/  puz  `path`(snoc `path`[pre i.paz] %hoon)
  ?^  (~(get by dir) puz)
    `puz
  $(paz t.paz)
::  preprocessed hoon file
++  pile
  $:  sur=(list taut)  ::  /-
      lib=(list taut)  ::  /+
      raw=(list [face=term =path])
      bar=(list [face=term mark-unsupported=@tas =path])
      =hoon
  ==
::
++  resolve-pile
  ::  turn fits into resolved path suffixes
  |=  [=pile dir=(map path cord)]
  ^-  rile
  %=  pile
    sur  (turn sur.pile |=(taut [face (need (get-fit %sur pax dir))]))
    lib  (turn lib.pile |=(taut [face (need (get-fit %lib pax dir))]))
      raw
    %+  turn  raw.pile
    |=  [face=term pax=path]
    [`face `path`(snoc pax %hoon)]
  ::
      bar
    %+  turn  bar.pile
    |=  [face=term mark-unsupported=@tas pax=path]
    !!
  ==
--
::
::  builder core
::
|_  honc=(trap vase)
::
++  import-graph
  $+  import-graph
  $~  [*path ~ ~ ~ ~ *(unit @tas) *hoon]  ::  not needed in the dojo but here for some reason
  $:  =path
      sur=(list import-graph)
      lib=(list import-graph)
      raw=(list import-graph)
      bar=(list import-graph)
      face=(unit @tas)  ::  the face that this node of the import graph has
      =hoon
  ==
::
::  +build-honc: build hoon.hoon separately to avoid recompiling whenever rebuilding kernel
++  build-honc
  ^-  (trap vase)
  (swat *(trap vase) (ream (crip hoon-139-txt)))
::
++  create
  |=  [entry=path dir=(map path cord)]
  ^-  (trap)
  =/  dir-hash  (mug dir)
  ~&  dir-hash+dir-hash
  =/  graph  (make-import-graph ~ entry 0 ~ dir)
  ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
  =;  ker-gen
    =>  (shot ker-gen |.(!>(dir-hash)))
    |.(+:^$)
  %-  head
  (compile-graph (head graph) ~)
::
++  make-import-graph
  |=  [face=(unit @tas) suf=path depth=@ cache=(map path import-graph) dir=(map path cord)]
  ^-  [import-graph (map path import-graph)]
  ~&  building-graph-for+[depth=depth suf]
  ?^  existing=(~(get by cache) suf)
    ~&  >  "reusing cached graph for {<suf>}"
    [u.existing(face face) cache]  ::  make sure to use the provided face
  =/  rile  (resolve-pile (parse-pile suf (get-hoon suf dir)) dir)
  =^  new-sur=(list import-graph)  cache
    %^  spin  sur.rile  cache
    |=  [raut cache=(map path import-graph)]
    (make-import-graph face pax +(depth) cache dir)
  =^  new-lib=(list import-graph)  cache
    %^  spin  lib.rile  cache
    |=  [raut cache=(map path import-graph)]
    (make-import-graph face pax +(depth) cache dir)
  =^  new-raw=(list import-graph)  cache
    %^  spin  raw.rile  cache
    |=  [raut cache=(map path import-graph)]
    (make-import-graph face pax +(depth) cache dir)
  =^  new-bar=(list import-graph)  cache
    %^  spin  bar.rile  cache
    |=  [raut cache=(map path import-graph)]
    (make-import-graph face pax +(depth) cache dir)
  =/  graph=import-graph
    :*  suf
        sur=new-sur
        lib=new-lib
        raw=new-raw
        bar=new-bar
        face
        hoon.rile
    ==
  =/  no-face=_graph
    graph(face `%no-cache-entry-face)
  :-  graph
  (~(put by cache) suf no-face)
::
++  compile-graph
  ::  accepts an import-graph and compiles it down to a vase
  ::
  |=  [graph=import-graph cache=(map path (trap vase))]
  ^-  [(trap vase) cache=(map path (trap vase))]
  |^
  ::  recursively compile each dependency then cons them all together
  ::  (base case is when both sur and lib are ~)
  ~&  "processing {<path.graph>}"
  ?^  existing=(~(get by cache) path.graph)
    ~&  >  "reusing cached vase for {<path.graph>}"
    [(label-vase u.existing face.graph) cache]
  =^  surs  cache   (spin sur.graph cache compile-graph)
  =^  libs  cache   (spin lib.graph cache compile-graph)
  =^  raws  cache   (spin raw.graph cache compile-graph)
  =^  bars  cache   (spin bar.graph cache compile-graph)
  =/  sur-all=(trap vase)  (roll p.surs slew)
  =/  lib-all=(trap vase)  (roll p.libs slew)
  =/  raw-all=(trap vase)  (roll p.raws slew)
  =/  bar-all=(trap vase)  (roll p.bars slew)
  =/  deps=(trap vase)
    ::  we must always make hoon.hoon available to each `hoon.graph`
    ::  in case it's not available on account of being hidden behind a face in other dependencies
    ::
    ::  TODO make sure there are no bunted vases in here
    =-  (roll - |=([v=(trap vase) a=(trap vase)] (slew a v)))
    %+  murn  ~[lib-all sur-all bar-all raw-all honc]
    |=  dep=(trap vase)
    ?:  =(*(trap vase) dep)  ~
    `dep
  ::  compile the current `hoon.graph` against its compiled dependencies
  ::
  =/  compiled=(trap vase)
    (swat deps hoon.graph)
  ::  cache the vase before adding the face so that alias can be handled jit when pulling from cache
  ::
  =.  cache     (~(put by cache) path.graph compiled)
  =.  compiled  (label-vase compiled face.graph)
  [compiled cache]
  ::
  ++  label-vase
    |=  [vaz=(trap vase) face=(unit @tas)]
    ^-  (trap vase)
    ?~  face  vaz
    (swat vaz (ream (crip :(weld "^=  " (scow %tas u.face) "  ."))))
  --
::
++  swel
  |=  [tap=(trap vase) gen=hoon]
  ^-  (trap vase)
  =/  gun  (~(mint ut p:$:tap) %noun gen)
  =>  [tap=tap gun=gun]
  |.  ~+
  =/  pro  q:$:tap
  [[%cell p.gun p:$:tap] [.*(pro q.gun) pro]]
::
++  slew
  |=  [hed=(trap vase) tal=(trap vase)]
  ^-  (trap vase)
  =>  +
  |.  ~+
  =+  [bed bal]=[$:hed $:tal]
  [[%cell p:bed p:bal] [q:bed q:bal]]
::  +shot: deferred slam
::
::  NOTE: this should never run inside of a trap. if it does, the builder
::  dependencies will leak into the result.
::
++  shot
  |=  [gat=(trap vase) sam=(trap vase)]
  ^-  (trap vase)
  =/  [typ=type gen=hoon]
    :-  [%cell p:$:gat p:$:sam]
    [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
  =+  gun=(~(mint ut typ) %noun gen)
  =>  [typ=p.gun +<.$]
  |.  ~+
  [typ .*([q:$:gat q:$:sam] [%9 2 %10 [6 %0 3] %0 2])]
::
++  get-hoon
  ::  produces the hoon source at the given path
  |=  [pax=path dir=(map path cord)]
  ^-  tape
  %-  trip
  (~(got by dir) pax)
::
++  is-graph-leaf
  |=  import-graph
  ^-  ?
  &(=(~ sur) =(~ lib))
--

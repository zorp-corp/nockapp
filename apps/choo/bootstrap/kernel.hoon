/+  *wrapper
=>
::  build system
::
=>
::
::  dependency system
::
|%
+$  octs  [p=@ud q=@]
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
+$  graph-leaf
  $%  [%hoon =hoon]
      [%octs =octs]
  ==
::
++  import-graph
  $+  import-graph
  $~  [*path ~ ~ ~ ~ *(unit @tas) [%hoon *hoon] *@]  ::  not needed in the dojo but here for some reason
  $:  =path
      sur=(list import-graph)
      lib=(list import-graph)
      raw=(list import-graph)
      bar=(list import-graph)
      face=(unit @tas)  ::  the face that this node of the import graph has
      leaf=graph-leaf
      hash=@            ::  shax hash of file cord
  ==
++  to-wain                                           ::  cord to line list
  |=  txt=cord
  ^-  wain
  ?~  txt  ~
  =/  len=@  (met 3 txt)
  =/  cut  =+(cut -(a 3, c 1, d txt))
  =/  sub  sub
  =|  [i=@ out=wain]
  |-  ^+  out
  =+  |-  ^-  j=@
      ?:  ?|  =(i len)
              =(10 (cut(b i)))
          ==
        i
      $(i +(i))
    =.  out  :_  out
    (cut(b i, c (sub j i)))
  ?:  =(j len)
    (flop out)
  $(i +(j))
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
      =/  =wain  (to-wain (crip tex))
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
::
++  get-fit
  |=  [pre=@ta pax=@tas dir=(map path cord)]
  ^-  (unit path)
  =/  paz=(list path)  (segments pax)
  |-
  ?~  paz  ~
  =/  last=term  (rear i.paz)
  =.  i.paz   `path`(snip i.paz)
  =/  puz
    ^-  path
    %+  snoc
      `path`[pre i.paz]
    `@ta`(rap 3 ~[last %'.' %hoon])
  ?^  (~(get by dir) puz)
    `puz
  $(paz t.paz)
::  preprocessed hoon file
++  pile
  $:  sur=(list taut)  ::  /-
      lib=(list taut)  ::  /+
      raw=(list [face=term =path])
      bar=(list [face=term mark=@tas =path])
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
    =/  pax-snip  (snip pax)
    =/  pax-rear  (rear pax)
    [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' %hoon]))]
  ::
      bar
    %+  turn  bar.pile
    |=  [face=term mark=@tas pax=path]
    ?:  =(mark %hoon)
      =/  pax-snip  (snip pax)
      =/  pax-rear  (rear pax)
      [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' %hoon]))]
    =/  pax-snip  (snip pax)
    =/  pax-rear  (rear pax)
    [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' mark]))]
  ==
--
::
::  builder core
::
|_  [honc=(trap vase) gra=(map path import-graph) tra=(map path (trap vase))]
::
::
::  $entry: path of a file along with unit of its contents.
::
::    If unit is null, the path must exist inside of the dir map.
::
+$  entry  [pat=path tex=(unit cord)]
++  build-honc
  |=  hoon-txt=cord
  ^-  (trap vase)
  (swet *(trap vase) (ream hoon-txt))
::
::
++  evict
  |=  [=entry dir=(map path import-graph)]
  ?~  tex.entry
    %|
  =/  new-hash=@  (shax u.tex.entry)
  ?.  (~(has by dir) pat.entry)
    ~&  "not in dir"
    ~&  keys+~(key by dir)
    ~&  path+pat.entry
    %|
  =/  old-hash=@  hash:(~(got by dir) pat.entry)
  ~&  "check hash match {<=(new-hash old-hash)>} for path: {<pat.entry>}"
  !=(new-hash old-hash)
::
++  create
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) (map path import-graph) (map path (trap vase))]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  ~&  keys-dir+~(key by dir)
  ~&  >>  "create"
  ~&  >>  gra-keys+~(key by gra)
  ~&  >>  tra-keys+~(key by tra)
  =/  res
    %+  roll
      ~(tap by dir)
    |=  [[pat=path cor=cord] [gra=_gra tra=_tra]]
    ?.  (evict [pat `cor] gra)
      [gra tra]
    (evict-cache pat (shax cor) gra tra)
  =.  gra  -.res
  =.  tra  +.res
  ?:  (evict entry gra)
    ?@  tex.entry
      !!
    =/  caches  (evict-cache pat.entry (shax u.tex.entry) gra tra)
    =/  graph  (make-import-graph ~ entry 0 -:caches dir)
    ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
    =/  compiled  (compile-graph (head graph) +:caches)
    :+  =>  %+  shot  (head compiled)
          =>(d=!>(dir-hash) |.(d))
        |.(+:^$)
      (tail graph)
    (tail compiled)
  =/  graph  (make-import-graph ~ entry 0 gra dir)
  ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
  =/  compiled  (compile-graph (head graph) tra)
  :+  =>  %+  shot  (head compiled)
        =>(d=!>(dir-hash) |.(d))
      |.(+:^$)
    (tail graph)
  (tail compiled)
::
::++  create
::  |=  [=entry dir=(map path cord)]
::  ^-  (trap)
::  =/  dir-hash  `@uvI`(mug dir)
::  ~&  dir-hash+dir-hash
::  =/  graph  (make-import-graph ~ entry 0 ~ dir)
::  ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
::  =;  ker-gen
::    =>  %+  shot  ker-gen
::        =>  d=!>(dir-hash)
::        |.(d)
::    |.(+:^$)
::  %-  head
::  (compile-graph (head graph) ~)
++  create-arbitrary
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) (map path import-graph) (map path (trap vase))]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  =/  graph  (make-import-graph ~ entry 0 ~ dir)
  =/  compiled  (compile-graph (head graph) ~)
  :+  =>(^-((trap vase) (head compiled)) |.(+:^$))
    (tail graph)
  (tail compiled)
::
++  get-file
  |=  [suf=entry dir=(map path cord)]
  ^-  cord
  ?~  tex.suf
    (~(got by dir) pat.suf)
  u.tex.suf
::
++  evict-cache
  |=  [pat=path ha=@ gra=(map path import-graph) tra=(map path (trap vase))]
  ^-  [gra=(map path import-graph) tra=(map path (trap vase))]
  ~&  >>  "evicting cache for pat: {<pat>}"
  =/  next=(list [path @])  ~[[pat ha]]
  |-
  ?~  next
    [gra tra]
  =/  [p=path ha=@]  (head next)
  ?.  (~(has by gra) p)
    $(next t.next)
  =/  ig  (~(got by gra) p)
  ?:  =(ha hash.ig)
    $(next t.next)
  ~&  >>  "deleting cache entry for {<p>}"
  =.  gra  (~(del by gra) p)
  =.  tra  (~(del by tra) p)
  =;  [gra=(map path import-graph) tra=(map path (trap vase)) next=(list [path @])]
    %=    $
      gra   gra
      tra   tra
      next  next
    ==
  ::  Check for other graphs that include the
  ::  Evicted file in their dependencies
  ::  Evict those if they do, and add them to the list
  ^-  [gra=(map path import-graph) tra=(map path (trap vase)) next=(list [path @])]
  %+  roll
    `(list import-graph)`~(val by gra)
  |=  [g=import-graph gra=_gra tra=_tra n=_^-((list [path @]) (tail next))]
  ~&  >>  "in {<path.g>}"
  =/  a  (turn lib.g |=(gg=import-graph ~&(path.gg path.gg)))
  ?:  ?|  (lien sur.g |=(gg=import-graph ~&(path.gg =(p path.gg))))
          (lien lib.g |=(gg=import-graph ~&(path.gg =(p path.gg))))
          (lien raw.g |=(gg=import-graph ~&(path.gg =(p path.gg))))
          (lien bar.g |=(gg=import-graph ~&(path.gg =(p path.gg))))
      ==
    ~&  >>  "(inner) deleting cache entry for {<path.g>}"
    :+   (~(del by gra) path.g)
      (~(del by tra) path.g)
    (snoc next [path.g hash.g])
  [gra tra next]
::
++  make-import-graph
  |=  [face=(unit @tas) suf=entry depth=@ cache=(map path import-graph) dir=(map path cord)]
  ^-  [import-graph (map path import-graph)]
  ~&  building-graph-for+[depth=depth pat.suf]
  ?^  existing=(~(get by cache) pat.suf)
    ~&  >  "reusing cached graph for {<pat.suf>}"
    [u.existing(face face) cache]  ::  make sure to use the provided face
  =/  file=cord  (get-file suf dir)
  ?.  (is-hoon pat.suf)
    =/  graph=import-graph
      :*  pat.suf
          ~  ~
          ~  ~
          face
          [%octs [(met 3 file) file]]
          (shax file)
      ==
    =/  no-face=_graph
      graph(face `%no-cache-entry-face)
    :-  graph
    (~(put by cache) pat.suf no-face)
  =/  rile  (resolve-pile (parse-pile pat.suf (trip file)) dir)
  =^  new-sur=(list import-graph)  cache
    %^  spin  sur.rile  cache
    |=  [raut cache=(map path import-graph)]
    (make-import-graph face [pax ~] +(depth) cache dir)
  =^  new-lib=(list import-graph)  cache
    %^  spin  lib.rile  cache
    |=  [raut cache=(map path import-graph)]
    =/  c  (~(got by dir) pax)
    (make-import-graph face [pax ~] +(depth) cache dir)
  =^  new-raw=(list import-graph)  cache
    %^  spin  raw.rile  cache
    |=  [raut cache=(map path import-graph)]
    =/  c  (~(got by dir) pax)
    (make-import-graph face [pax ~] +(depth) cache dir)
  =^  new-bar=(list import-graph)  cache
    %^  spin  bar.rile  cache
    |=  [raut cache=(map path import-graph)]
    =/  c  (~(got by dir) pax)
    (make-import-graph face [pax ~] +(depth) cache dir)
  =/  graph=import-graph
    :*  pat.suf
        sur=new-sur
        lib=new-lib
        raw=new-raw
        bar=new-bar
        face
        [%hoon hoon.rile]
        (shax file)
    ==
  =/  no-face=_graph
    graph(face `%no-cache-entry-face)
  :-  graph
  (~(put by cache) pat.suf no-face)
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
    %+  murn  ~[lib-all sur-all raw-all bar-all honc]
    |=  dep=(trap vase)
    ?:  =(*(trap vase) dep)  ~
    `dep
  ::  compile the current `hoon.graph` against its compiled dependencies
  ::
  =/  compiled=(trap vase)
    ?:  ?=(%hoon -.leaf.graph)
      (swet deps hoon.leaf.graph)
    =>  octs=!>(octs.leaf.graph)
    |.  octs
  ~&  compiled+path.graph
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
    =>  [vaz=vaz face=u.face]
    |.
    =/  vas  $:vaz
    [[%face face p.vas] q.vas]
  --
::
++  slew
  |=  [hed=(trap vase) tal=(trap vase)]
  ^-  (trap vase)
  =>  +<
  |.
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
  |.
  [typ .*([q:$:gat q:$:sam] [%9 2 %10 [6 %0 3] %0 2])]
::
::  +swet: deferred +slap
::  NOTE: this is +swat but with a bug fixed that caused a space leak in
::  the resulting trap vases.
::
++  swet
  |=  [tap=(trap vase) gen=hoon]
  ^-  (trap vase)
  =/  gun  (~(mint ut p:$:tap) %noun gen)
  =>  [gun=gun tap=tap]
  |.  ~+
  [p.gun .*(q:$:tap q.gun)]
::
++  is-hoon
  |=  pax=path
  ^-  ?
  =/  end  (rear pax)
  !=(~ (find ".hoon" (trip end)))
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
=>
|%
+$  choo-state
  $%  [%0 cached-hoon=(unit (trap vase)) ~]
      $:  %1
          cached-hoon=(unit (trap vase))
          gra=(map path import-graph)
          tra=(map path (trap vase))
      ==
  ==
::
++  moat  (keep choo-state)
+$  cause
  $%  [%build pat=cord tex=cord directory=(list [cord cord]) arbitrary=?]
      [%file %write path=@t contents=@ success=?]
      [%boot hoon-txt=cord]
  ==
+$  effect
  $%  [%file %write path=@t contents=@]
      [%exit id=@]
  ==
--
::
~&  %choo-choo
%-  moat
^-  fort:moat
|_  k=choo-state
+*  builder  +>
::
::  +load: upgrade from previous state
::
++  load
  |=  arg=choo-state
  ?.  ?=(%0 -.arg)
    ?>  ?=(%1 -.arg)
    arg
  [%1 cached-hoon.arg ~ ~]
::
::  +peek: external inspect
::
++  peek
  |=  =path
  ^-  (unit (unit *))
  ``?=(^ cached-hoon.k)
::
::  +poke: external apply
::
++  poke
  |=  [eny=@ our=@ux now=@da dat=*]
  ^-  [(list effect) choo-state]
  =/  cause=(unit cause)  ((soft cause) dat)
  ?~  cause
    ~&  >>  "input is not a proper cause"
    !!
  =/  cause  u.cause
  ?-    -.cause
      %file
    [~ k]
  ::
      %boot
    ~&  hoon-version+hoon-version
    ?:  ?=(^ cached-hoon.k)
      [~ k]
   [~ k(cached-hoon `(build-honc hoon-txt.cause))]
  ::
      %build
    =/  =entry  [(stab pat.cause) `tex.cause]
    =/  dir
      %-  ~(gas by *(map path cord))
      (turn directory.cause |=((pair @t @t) [(stab p) q]))
    ~&  >>  "new build"
    ?>  ?=(^ cached-hoon.k)
    ?>  ?=(%1 -.k)
    =/  res
      ?:  arbitrary.cause
        %-  ~(create-arbitrary builder u.cached-hoon.k gra.k tra.k)
        [entry dir]
      %-  ~(create builder u.cached-hoon.k gra.k tra.k)
      [entry dir]
    =/  contents  (jam (head res))
    :_  k(gra +6:res, tra +7:res)
    :~  :*  %file
            %write
            path=(crip "out.jam")
            contents=contents
        ==
        [%exit 0]
    ==
  ==
--
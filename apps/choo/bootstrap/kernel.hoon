/+  *wrapper
=>
|%
+$  choo-state  [%0 cached-hoon=(unit (trap vase)) =build-cache]
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
::
::  $entry: path of a file along with unit of its contents.
::
::    If unit is null, the path must exist inside of the dir map.
::
+$  entry  [pat=path tex=(unit cord)]
::
+$  hash  @
+$  build-cache  (map hash (trap vase))
--
::
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
  |=  arg=choo-state
  arg
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
    ?>  ?=(^ cached-hoon.k)
    =/  [compiled=* bc=build-cache]
      ?:  arbitrary.cause
        %-  ~(create-arbitrary builder u.cached-hoon.k build-cache.k)
        [entry dir]
      %-  ~(create builder u.cached-hoon.k build-cache.k)
      [entry dir]
    :_  k(build-cache bc)
    :~  :*  %file
            %write
            path=(crip "out.jam")
            contents=(jam compiled)
        ==
        [%exit 0]
    ==
  ==
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
::  +$  taut  [face=(unit term) pax=term]
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
  ^-  (list raut)
  ;:  weld
    (turn sur.pile |=(taut ^-(raut [face (need (get-fit %sur pax dir))])))
    (turn lib.pile |=(taut ^-(raut [face (need (get-fit %lib pax dir))])))
  ::
    %+  turn  raw.pile
    |=  [face=term pax=path]
    =/  pax-snip  (snip pax)
    =/  pax-rear  (rear pax)
    ^-  raut
    [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' %hoon]))]
  ::
    %+  turn  bar.pile
    |=  [face=term mark=@tas pax=path]
    ?:  =(mark %hoon)
      =/  pax-snip  (snip pax)
      =/  pax-rear  (rear pax)
      ^-  raut
      [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' %hoon]))]
    =/  pax-snip  (snip pax)
    =/  pax-rear  (rear pax)
    ^-  raut
    [`face `path`(snoc pax-snip `@ta`(rap 3 ~[pax-rear %'.' mark]))]
  ==
--
::
::  builder core
::
|_  [honc=(trap vase) bc=build-cache]
::
++  build-honc
  |=  hoon-txt=cord
  ^-  (trap vase)
  (swet *(trap vase) (ream hoon-txt))
::
+$  octs  [p=@ud q=@]
::
::
+$  temp-cache  (map path [hash=@ vaz=(trap vase)])
::
::  $node: entry of adjacency matrix with metadata
::
::  Holds the outgoing edges
+$  node
  $:  =path
      hash=@
      deps=(list raut)
      =hoon
  ==
::
::  $node-set: glorified adjacency matrix
+$  node-set
  $:  target=node
      map=(map path node)
      leaves=(map path node)
  ==
::
::  $graph-view: adjacency matrix with easier access to neighbors
::
::  Used to keep track of traversal when building the merkle DAG
+$  graph-view  (map path (set path))
::
++  create
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) build-cache]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  =/  ns  (make-node-set entry dir)
  =/  compile  (compile-node-set ns)
  ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
  =/  ker-gen  (head compile)
  :_  +7:compile
  =>  %+  shot  ker-gen
      =>  d=!>(dir-hash)
      |.(d)
  |.(+:^$)
::
++  create-arbitrary
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) build-cache]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  =/  ns  (make-node-set entry dir)
  =/  compile  (compile-node-set ns)
  :_  +7:compile
  =>  (head compile)
  |.(+:^$)
::
++  get-file
  |=  [suf=entry dir=(map path cord)]
  ^-  cord
  ?~  tex.suf
    (~(got by dir) pat.suf)
  u.tex.suf
::
++  get-deps
  |=  [n=node dir=(map path cord) seen=(map path node)]
  ^-  (list [path cord])
  |^
  (murn deps.n take)
  ::
  ++  take
    |=  raut
    ^-  (unit [path cord])
    ::  Note: if it has been seen in the BFS, maybe that means there has been a cycle?
    ?:  (~(has by seen) pax)
      ~
    ?.  (~(has by dir) pax)
      ~&  "Could not find dependency {<pax>} for {<path.n>}"  !!
    `[pax (~(got by dir) pax)]
  --
::
::  +is-leaf: Checks if a rile has no dependencies
::
++  is-leaf
  |=  node
  .=(~ deps)
::
++  build-graph-view
  |=  ns=node-set
  ^-  graph-view
  %-  ~(urn by map.ns)
  |=  [* n=node]
  %-  silt
  (turn deps.n |=(raut pax))
::  Returns an augmented adjacency graph made from all the files required
::  to build the suf. Does this via BFS.
::
++  make-node-set
  |=  [suf=entry dir=(map path cord)]
  ^-  node-set
  ?~  tex.suf  !!
  =/  target  (make-node pat.suf u.tex.suf dir)
  =/  curr  target
  =/  deps=(list [path cord])  (get-deps target dir ~)
  =/  ns=node-set  [target ~ ~]
  |-
  ?:  =((lent deps) 0)
    ns
  =/  [ns=node-set deps=_deps]
    %+  roll
      deps
    |=  [[pat=path tex=cord] [ns=_ns deps=(list [path cord])]]
    ?:  (~(has by map.ns) pat)
      [ns deps]
    =/  n=node  (make-node pat tex dir)
    =.  ns  ns(map (~(put by map.ns) path.n n))
    =?  ns  (is-leaf n)
      ns(leaves (~(put by leaves.ns) path.n n))
    :-  ns
    (weld deps (get-deps n dir map.ns))
  $(ns ns, deps deps)
::
++  make-node
  |=  [pat=path file=cord dir=(map path cord)]
  ^-  node
  ~&  building-graph-for+pat
  =/  pile  (parse-pile pat (trip file))
  =/  deps=(list raut)  (resolve-pile pile dir)
  :*  path=pat
      hash=(shax file)
      deps=deps
      hoon=hoon.pile
  ==
::
::  To compile a node set, we just need to compile along a topological sorting
::  of the nodes
++  compile-node-set
  |=  ns=node-set
  ^-  [(trap vase) temp-cache build-cache]
  |^
  =|  new-bc=build-cache
  =/  graph  (build-graph-view ns)
  =|  tc=temp-cache
  =/  next=(map path node)  leaves.ns
  ::
  ::  bopological sort
  |-
  ~&  >>  traversing+~(key by next)
  ~&  >>  graph-view+graph
  ?:  .=(~ next)
    (compile-node target.ns tc new-bc)
  =-
    %=  $
      next   (update-next ns graph)
      graph  graph
      tc     tc
      new-bc     new-bc
    ==
  ^-  [graph=(map path (set path)) tc=temp-cache new-bc=build-cache]
  %+  roll
    ~(tap by next)
  |=  [[p=path n=node] graph=_graph tc=_tc new-bc=_new-bc]
  =.  graph  (update-graph-view graph p)
  :-  graph
  ::  returns temp-cache, build-cache
  +:(compile-node n tc new-bc)
::
  ::  TODO clean up the wuts
  ++  update-next
    |=  [ns=node-set gv=graph-view]
    ^-  (map path node)
    %+  roll
      ~(tap by gv)
    |=  [[pax=path edges=(set path)] next=(map path node)]
    ::  if we don't have the entry in gv, already visited
    ::
    :: if a node has out edges, do not add it to next
    ?.  =(*(set path) edges)
      next
    %+  ~(put by next)
      pax
    (~(got by map.ns) pax)
::
  ++  update-graph-view
    |=  [gv=graph-view p=path]
    ^-  graph-view
    =.  gv  (~(del by gv) p)
    %-  ~(urn by gv)
    |=  [* edges=(set path)]
    (~(del in edges) p)
::

  ++  compile-node
    |=  [n=node tc=temp-cache new-bc=build-cache]
    ^-  [(trap vase) temp-cache build-cache]
    ~&  >>  compiling-node+path.n
    ~&  >>  cache-keys+~(key by tc)
    =;  [vaz-deps=(trap vase) hash=@]
      =.  vaz-deps  (slew vaz-deps honc)
      =/  target=(trap vase)
        ?:  (~(has by bc) hash)
          ~&  >>  cache-hit+path.n
          (~(got by bc) hash)
        ~&  >>  cache-miss+path.n
        (swet vaz-deps hoon.n)
      :*  target
          (~(put by tc) path.n [hash target])
          (~(put by new-bc) hash target)
      ==
    %+  roll
      deps.n
    |=  [raut vaz=(trap vase) hash=_hash.n]
    ~&  >>  grabbing+pax
    =/  [dep-hash=@ dep-vaz=(trap vase)]  (~(got by tc) pax)
    ~&  >>  grabbed+pax
    :-  (slew vaz (label-vase dep-vaz face))
    (shax (rep 8 ~[hash dep-hash]))
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
--

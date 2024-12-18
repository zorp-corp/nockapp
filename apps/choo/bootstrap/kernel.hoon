/+  *wrapper
=>
|%
+$  state-0  [%0 cached-hoon=(unit (trap vase)) ~]
+$  state-1  [%1 cached-hoon=(unit (trap vase)) bc=build-cache pc=parse-cache]
+$  versioned-state
  $%  state-0
      state-1
  ==
+$  choo-state  state-1
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
::
::  $taut: file import from /lib or /sur
::
+$  taut  [face=(unit term) pax=term]
::
::  $pile:  preprocessed hoon file
::
+$  pile
  $:  sur=(list taut)  ::  /-
      lib=(list taut)  ::  /+
      raw=(list [face=term =path])
      bar=(list [face=term mark=@tas =path])
      =hoon
  ==
::
::  $parse-cache: content addressed cache of preprocessed hoon files.
::
+$  parse-cache  (map hash pile)
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
::
++  load
  |=  arg=versioned-state
  ^-  choo-state
  ?+    -.arg    ~&  >>  %no-upgrade  arg
      %0
    ~&  >>  %upgrade-0-to-1
    :*  %1
        cached-hoon.arg
        *build-cache
        *parse-cache
    ==
  ==
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
    ~&  >>>  "input is not a proper cause"
    !!
  =/  cause  u.cause
  ?-    -.cause
      %file
    [~ k]
  ::
      %boot
    ~&  >>  hoon-version+hoon-version
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
    =/  [compiled=* new-bc=build-cache new-pc=parse-cache]
      ?:  arbitrary.cause
        %-  ~(create-arbitrary builder u.cached-hoon.k bc.k pc.k)
        [entry dir]
      %-  ~(create builder u.cached-hoon.k bc.k pc.k)
      [entry dir]
    :_  k(bc new-bc, pc new-pc)
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
|_  [honc=(trap vase) bc=build-cache pc=parse-cache]
::
++  build-honc
  |=  hoon-txt=cord
  ^-  (trap vase)
  (swet *(trap vase) (ream hoon-txt))
::
+$  octs  [p=@ud q=@]
::
::  $node: entry of adjacency matrix with metadata
::
+$  node
  $:  =path
      hash=@
      ::  holds only outgoing edges
      deps=(list raut)
      =hoon
  ==
::
::  $graph-view: adjacency matrix with easier access to neighbors
::
::    used to keep track of traversal when building the merkle DAG
::
+$  graph-view  (map path (set path))
::
++  create
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) build-cache parse-cache]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  >>  dir-hash+dir-hash
  =/  [parsed-dir=(map path node) pc=parse-cache]  (parse-dir entry dir)
  =/  [dep-dag=merk-dag =path-dag]  (build-merk-dag parsed-dir)
  ~&  >>  dep-dag+dep-dag
  ~&  >>  path-dag+path-dag
  *[(trap) build-cache parse-cache]
  ::
  ::  delete invalid cache entries in bc
  ::=.  bc
  ::  %+  roll
  ::    ~(tap by bc)
  ::  |=  [[hash=@ *] bc=_bc]
  ::  ?:  (~(has by dep-dag) hash)
  ::    bc
  ::  (~(del by bc) hash)
  ::=/  compile
  ::  %:  compile-target
  ::    pat.entry
  ::    path-dag
  ::    (get-build-deps entry parsed-dir)
  ::    bc
  ::  ==
  ::::  +shot calls the kernel gate to tell it the hash of the zkvm desk
  ::=/  ker-gen  (head compile)
  :::_  [(tail compile) pc]
  ::=>  %+  shot  ker-gen
  ::    =>  d=!>(dir-hash)
  ::    |.(d)
  ::|.(+:^$)
::
::
++  create-arbitrary
  |=  [=entry dir=(map path cord)]
  ^-  [(trap) build-cache parse-cache]
  =/  dir-hash  `@uvI`(mug dir)
  ~&  >>  dir-hash+dir-hash
  =/  [parsed-dir=(map path node) pc=parse-cache]  (parse-dir entry dir)
  =/  [dep-dag=merk-dag =path-dag]  (build-merk-dag parsed-dir)
  ~&  >>  dep-dag+dep-dag
  *[(trap) build-cache parse-cache]
::
++  parse-dir
  |=  [suf=entry dir=(map path cord)]
  ^-  [(map path node) parse-cache]
  =/  [suf-node=node new-pc=parse-cache]
    (make-node pat.suf (need tex.suf) dir ~)
  =|  nodes=(map path node)
  =.  nodes  (~(put by nodes) pat.suf suf-node)
  %+  roll
    ~(tap by dir)
  |=  [[pat=path file=cord] nodes=_nodes new-pc=_new-pc]
  =^  n=node  new-pc
    (make-node pat file dir new-pc)
  :-  (~(put by nodes) pat n)
  new-pc
::
++  make-node
  |=  [pat=path file=cord dir=(map path cord) new-pc=parse-cache]
  ^-  [node parse-cache]
  ~&  >  building-graph-for+pat
  =/  hash=@  (shax file)
  =/  =pile
    ?:  (~(has by pc) hash)
      ~&  >  parse-cache-hit+pat
      (~(got by pc) hash)
    ~&  >  parse-cache-miss+pat
    (parse-pile pat (trip file))
  :_  (~(put by new-pc) hash pile)
  :*  path=pat
      hash=(shax file)
      deps=(resolve-pile pile dir)
      hoon=hoon.pile
  ==
::
::  combine dep-dag and merk-dag into one structure
::  recursively compile each dependency
++  compile-graph
  ::  accepts an import-graph and compiles it down to a vase
  ::
  |=  [n=node dep-dag=merk-dag =path-dag cache=build-cache]
  ^-  [(trap vase) cache=(map path (trap vase))]
  |^
  ::  recursively compile each dependency then cons them all together
  ::  (base case is when both sur and lib are ~)
  ~&  "processing {<path.n>}"
  ?^  existing=(~(get by cache) path.n)
    ~&  >  "reusing cached vase for {<path.n>}"
    [(label-vase u.existing face.n) cache]
  ::  get nodes
  ::=/  ns=(list node)
  ::  spin nodes
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
::  $get-build-deps: Builds adjacency matrix.
::
::    Gathers dependencies of the build target via breadth-first-search.
::
++  get-build-deps
  |=  [suf=entry nodes=(map path node)]
  ^-  (map path node)
  |^
  ?~  tex.suf  !!
  =|  target-deps=(map path node)
  =|  new-pc=parse-cache
  =/  target=node  (~(got by nodes) pat.suf)
  =/  deps=(list [path node])  (get-deps target nodes)
  |-
  ?:  .=(deps ~)
    target-deps
  =;  [target-deps=(map path node) deps=(list [path node])]
    $(target-deps target-deps, deps deps)
  %+  roll
    deps
  |=  [[pat=path n=node] [target-deps=(map path node) deps=(list [path node])]]
  ?:  (~(has by nodes) pat)
    [target-deps deps]
  :-  (~(put by target-deps) pat n)
  (weld deps (get-deps n nodes))
  ::
  ++  get-deps
    |=  [n=node nodes=(map path node)]
    ^-  (list [path node])
    |^
    (turn deps.n take)
    ::
    ++  take
      |=  raut
      ^-  [path node]
      ?.  (~(has by nodes) pax)
        ~&  >>>  "Could not find dependency {<pax>} for {<path.n>}"  !!
      [pax (~(got by nodes) pax)]
    --
  --
::
::++  compile-target
::  |=  [pat=path =path-dag nodes=(map path node) bc=build-cache]
::  ^-  [(trap vase) build-cache]
::  =/  n=node  +:(~(got by path-dag) pat)
::  =/  graph  (build-graph-view nodes)
::  =/  next=(map path node)  (update-next nodes graph)
::  =|  vaz=(trap vase)
::  |-
::  ~&  >  traversing+~(key by next)
::  ~&  >  graph-view+graph
::  ?:  .=(~ next)
::    (compile-node n path-dag bc)
::  =-
::    %=  $
::      next   (update-next nodes graph)
::      graph  graph
::      bc     bc
::    ==
::  ^-  [graph=(map path (set path)) bc=build-cache]
::  %+  roll
::    ~(tap by next)
::  |=  [[p=path n=node] graph=_graph bc=_bc]
::  =^  *  bc
::    (compile-node n path-dag bc)
::  [(update-graph-view graph p) bc]
::::
::++  compile-node
::  |=  [n=node =path-dag bc=build-cache]
::  ^-  [(trap vase) build-cache]
::  ~&  >  compiling-node+path.n
::  ~&  >  cache-keys+~(key by bc)
::  =/  [dep-hash=@ *]  (~(got by path-dag) path.n)
::  ?:  (~(has by bc) dep-hash)
::    ~&  >  build-cache-hit+path.n
::    [(~(got by bc) dep-hash) bc]
::  ~&  >  build-cache-miss+path.n
::  =/  vaz=(trap vase)  (build-node n path-dag bc)
::  [vaz (~(put by bc) dep-hash vaz)]
::::
::++  build-node
::  |=  [n=node =path-dag bc=build-cache]
::  ^-  (trap vase)
::  =;  dep-vaz=(trap vase)
::    (swet dep-vaz hoon.n)
::  %+  roll
::    deps.n
::  |=  [raut vaz=(trap vase)]
::  ~&  >  grabbing-dep+pax
::  =/  [dep-hash=@ *]  (~(got by path-dag) pax)
::  =/  dep-vaz=(trap vase)  (~(got by bc) dep-hash)
::  (slew vaz (label-vase dep-vaz face))
::
++  label-vase
  |=  [vaz=(trap vase) face=(unit @tas)]
  ^-  (trap vase)
  ?~  face  vaz
  =>  [vaz=vaz face=u.face]
  |.
  =/  vas  $:vaz
  [[%face face p.vas] q.vas]
::
::  $build-merk-dag: builds a merkle DAG out of the dependency folder
::
::    To build the DAG, we compile the dependencies and subtree hashes along a topological sorting
::    of the node-set.
::
+$  merk-dag  (map @ node)
+$  path-dag  (map path [@ node])
::
++  build-merk-dag
  ::
  ::  node set of entire dir + target
  |=  nodes=(map path node)
  ^-  [merk-dag path-dag]
  ::
  ::  Need a way to uniquely identify dep directories
  =|  dep-dag=merk-dag
  =|  =path-dag
  =/  graph  (build-graph-view nodes)
  =/  next=(map path node)  (update-next nodes graph)
  ::
  ::  traverse via a topological sorting of DAG
  |-
  ~&  >  traversing+~(key by next)
  ~&  >  graph-view+graph
  ?:  .=(~ next)
    [dep-dag path-dag]
  =-
    %=  $
      next   (update-next nodes graph)
      graph  graph
      dep-dag  dd
      path-dag  pd
    ==
  ^-  [graph=(map path (set path)) dd=(map @ node) pd=^path-dag]
  %+  roll
    ~(tap by next)
  |=  [[p=path n=node] graph=_graph dep-dag=_dep-dag path-dag=_path-dag]
  =/  hash  (calculate-hash n dep-dag path-dag)
  :+  (update-graph-view graph p)
    (~(put by dep-dag) hash n)
  (~(put by path-dag) p [hash n])
  ::
++  update-next
  |=  [nodes=(map path node) gv=graph-view]
  ^-  (map path node)
  ::
  ::  if we don't have the entry in gv, already visited
  %+  roll
    ~(tap by gv)
  |=  [[pax=path edges=(set path)] next=(map path node)]
  ::
  :: if a node has no out edges, add it to next
  ?.  =(*(set path) edges)
    next
  %+  ~(put by next)
    pax
  (~(got by nodes) pax)
::
++  update-graph-view
  |=  [gv=graph-view p=path]
  ^-  graph-view
  =.  gv  (~(del by gv) p)
  %-  ~(urn by gv)
  |=  [* edges=(set path)]
  (~(del in edges) p)
::
++  calculate-hash
  |=  [n=node dep-dag=merk-dag =path-dag]
  ^-  @
  ~&  >  calculating-hash+path.n
  %+  roll
    deps.n
  |=  [raut hash=_hash.n]
  ~&  >  grabbing-dep+pax
  ?.  (~(has by path-dag) pax)
    ~&  >>>  "calculate-hash: Missing {<pax>}"  !!
  =/  [dep-hash=@ *]
    (~(got by path-dag) pax)
  (shax (rep 8 ~[hash dep-hash]))
::
++  build-graph-view
  |=  nodes=(map path node)
  ^-  graph-view
  %-  ~(urn by nodes)
  |=  [* n=node]
  %-  silt
  (turn deps.n |=(raut pax))
::
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

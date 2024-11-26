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
    =/  contents=@
      %-  jam
      ?:  arbitrary.cause
        %-  ~(create-arbitrary builder u.cached-hoon.k)
        [entry dir]
      %-  ~(create builder u.cached-hoon.k)
      [entry dir]
    :_  k
    :~  :*  %file
            %write
            path=(crip "out.jam")
            contents=contents
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
|_  honc=(trap vase)
::
++  build-honc
  |=  hoon-txt=cord
  ^-  (trap vase)
  (swet *(trap vase) (ream hoon-txt))
::
+$  octs  [p=@ud q=@]
::
::
+$  temp-cache  (map path (trap vase))
::
::  $node: entry of adjacency matrix with metadata
::
::  A rile holds the outgoing edges
+$  node
  $:  pat=path
      face=(unit @tas)
      hash=@
      rile=rile
  ==
::
::  $node-set: glorified adjacency matrix
+$  node-set
  $:  target=node
      map=(map path node)
  ==
::
++  create
  |=  [=entry dir=(map path cord)]
  ^-  (trap)
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  =/  graph  (make-node-set entry dir)
  ~&  >>  gra+graph
  |.(42)
  ::  +shot calls the kernel gate to tell it the hash of the zkvm desk
  ::=;  ker-gen
  ::  =>  %+  shot  ker-gen
  ::      =>  d=!>(dir-hash)
  ::      |.(d)
  ::  |.(+:^$)
  ::%-  head
  ::(compile-graph (head graph) ~)
++  create-arbitrary
  |=  [=entry dir=(map path cord)]
  ^-  (trap)
  =/  dir-hash  `@uvI`(mug dir)
  ~&  dir-hash+dir-hash
  =/  graph  (make-node-set entry dir)
  |.(42)
  ::=/  tase
  ::  %-  head
  ::  (compile-graph (head graph) ~)
  ::=>  tase
  ::|.(+:^$)
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
  ;:  weld
    (murn sur.rile.n take)
    (murn lib.rile.n take)
    (murn raw.rile.n take)
    (murn bar.rile.n take)
  ==
  ::
  ++  take
  |=  raut
  ^-  (unit [path cord])
  ?:  (~(has by seen) pax)
    ~
  ?.  (~(has by dir) pax)
    ~&  "could not find dependency {<pax>} for {<pat.n>}"  !!
  `[pax (~(got by dir) pax)]
  --
::
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
  =/  ns=node-set  [target ~]
  |-
  ?:  =((lent deps) 0)
    ns
  =/  [ns=node-set deps=_deps]
    %+  roll
      deps
    |=  [[pat=path tex=cord] [ns=_ns deps=(list [path cord])]]
    =/  n=node  (make-node pat tex dir)
    ~&  >>  node+n
    =.  ns  ns(map (~(put by map.ns) pat.n n))
    :-  ns
    (weld deps (get-deps n dir map.ns))
  $(ns ns, deps deps)
::
++  make-node
  |=  [pat=path file=cord dir=(map path cord)]
  ^-  node
  ~&  building-graph-for+pat
  =/  pile  (parse-pile pat (trip file))
  =/  rile  (resolve-pile pile dir)
  ~&  >>  rile+rile
  :*  path=pat
      ::
      ::  what is this for??? maybe delete laterr
      ::
      face=`%no-cache-entry-face
      hash=(hash file)
      rile=rile
  ==
::
::  To compile a node set, we just need to compile along a topological sorting
::  of the nodes
::++  compile-node-set
::  |=  [ns=node-set cache=build-cache]
::  ^-  [(trap vase) temp-cache build-cache]
::  =/  temp-cache  (map path (trap vase))
::  %+  roll
::    (range max.ns)
::  |=  [i=@ [cache=_cache temp=_temp-cache]]
::  ?.  (~(has by temp) i)
::    [cache temp]
::  %+  roll
::    (~(got by ns) i)
::  |=  [nod=node [cache=_cache temp=_temp]]
::  ::  check cache first
::  ?:  (~(has by cache) pat.nod)
::    [cache temp]
::  ::  if we need to build, first get dependencies
::  ::  they should be in temp, keyed by their path if they are not,
::  ::  CRASH.
::  ::  slew them all together
::  ::  build the dependency
::  ::  add the face that is needed
::  ::  create the hash to key it by while building the slew
::  (spin sur.rile.nod)
::  ::  build the file
::  ::
::  ::  store it in the persisted cache, keyed by hash
::  ::  store it in the temp (within build) cache, keyed by path
::  ::  STORE IN THE TEMP GRAPH AS FACELESS
::::
::++  compile-graph
::  ::  accepts an import-graph and compiles it down to a vase
::  ::
::  |=  [graph=import-graph cache=(map path (trap vase))]
::  ^-  [(trap vase) cache=(map path (trap vase))]
::  |^
::  ::  recursively compile each dependency then cons them all together
::  ::  (base case is when both sur and lib are ~)
::  ~&  "processing {<path.graph>}"
::  ?^  existing=(~(get by cache) path.graph)
::    ~&  >  "reusing cached vase for {<path.graph>}"
::    [(label-vase u.existing face.graph) cache]
::  =^  surs  cache   (spin sur.graph cache compile-graph)
::  =^  libs  cache   (spin lib.graph cache compile-graph)
::  =^  raws  cache   (spin raw.graph cache compile-graph)
::  =^  bars  cache   (spin bar.graph cache compile-graph)
::  =/  sur-all=(trap vase)  (roll p.surs slew)
::  =/  lib-all=(trap vase)  (roll p.libs slew)
::  =/  raw-all=(trap vase)  (roll p.raws slew)
::  =/  bar-all=(trap vase)  (roll p.bars slew)
::  =/  deps=(trap vase)
::    ::  we must always make hoon.hoon available to each `hoon.graph`
::    ::  in case it's not available on account of being hidden behind a face in other dependencies
::    ::
::    ::  TODO make sure there are no bunted vases in here
::    =-  (roll - |=([v=(trap vase) a=(trap vase)] (slew a v)))
::    %+  murn  ~[lib-all sur-all raw-all bar-all honc]
::    |=  dep=(trap vase)
::    ?:  =(*(trap vase) dep)  ~
::    `dep
::  ::  compile the current `hoon.graph` against its compiled dependencies
::  ::
::  =/  compiled=(trap vase)
::    ?:  ?=(%hoon -.leaf.graph)
::      (swet deps hoon.leaf.graph)
::    =>  octs=!>(octs.leaf.graph)
::    |.  octs
::  ~&  compiled+path.graph
::  ::  cache the vase before adding the face so that alias can be handled jit when pulling from cache
::  ::
::  =.  cache     (~(put by cache) path.graph compiled)
::  =.  compiled  (label-vase compiled face.graph)
::  [compiled cache]
::  ::
::  ++  label-vase
::    |=  [vaz=(trap vase) face=(unit @tas)]
::    ^-  (trap vase)
::    ?~  face  vaz
::    =>  [vaz=vaz face=u.face]
::    |.
::    =/  vas  $:vaz
::    [[%face face p.vas] q.vas]
::  --
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
::++  is-graph-leaf
::  |=  import-graph
::  ^-  ?
::  &(=(~ sur) =(~ lib))
--

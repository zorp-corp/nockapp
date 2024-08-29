::/+  *wrapper
=>
|%
+$  server-state  %stateless
++  moat  (keep server-state)
+$  header  [k=@t v=@t]
+$  octs  [p=@ q=@]
+$  method
  $?  %'GET'
      %'PUT'
      %'POST'
  ==
+$  cause
  $:  %req
      uri=@t
      =method
      headers=(list header)
      body=(unit octs)
  ==
::
+$  effect
  $:  %res
      status=@ud
      headers=(list header)
      body=(unit octs)
  ==
--
::
~&  %serving
=<  $
%-  moat
^-  fort:moat
|_  k=server-state
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
  ^-  [(list effect) server-state]
  =/  sof-cau=(unit cause)  ((soft cause) dat)
  ?~  sof-cau
    ~&  "cause incorrectly formatted!"
    !!
  =/  =cause  u.sof-cau
  ~&  cause+cause
  :_  k
  :_  ~
  ^-  effect
  =-  ~&  effect+-
      -
  ?+    method.cause  [%res %400 ~ ~]
      %'GET'
    :^  %res  %200
      ~
    =+  %hi
    `[(met 1 -) -]
  ==
--


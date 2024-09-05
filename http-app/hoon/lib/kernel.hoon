/+  *wrapper
=>
|%
+$  server-state  %stateless
++  moat  (keep server-state)
+$  header  [k=@t v=@t]
+$  octs  [p=@ q=@]
+$  method
  $?  %'GET'
      %'HEAD'
      %'POST'
      %'PUT'
      %'DELETE'
      %'CONNECT'
      %'OPTIONS'
      %'TRACE'
      %'PATCH'
  ==
::
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
::
++  to-octs
  |=  bod=@
  ^-  (unit octs)
  =/  len  (met 3 bod)
  ?:  =(len 0)  ~
  `[len bod]
--
::
~&  %serving
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
  |=  =path
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
    ~&  dat
    !!
  =/  [uri=@t =method headers=(list header) body=(unit octs)]  +.u.sof-cau
  ~&  [uri+uri method+method headers+headers]
  :_  k
  :_  ~
  ^-  effect
  =-  ~&  effect+-
      -
  ?+    method  [%res %400 ~ ~]
      %'GET'
    :^  %res  %200
      ['content-type' 'text/html']~
    %-  to-octs
    '''
    <!doctype html>
    <html>
      <body>
        <h1>Hello NockApp!</h1>
      </body>
    </html>
    '''
  ::
      %'POST'
    !!
  ==
--


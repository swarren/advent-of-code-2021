begin-structure dot-node
  field: dot-node-x
  field: dot-node-y
  field: dot-node-next
end-structure

: dump-list-dots
    begin
        @
        dup 0= invert
    while
        ." dot: "
        dup dot-node-x @ decimal .
        dup dot-node-y @ decimal .
        cr
        dot-node-next
    repeat
    drop
    ;

: count-list-dots
    ( list-dots/dot-node-ptr-ptr )
    0 >r
    begin
        ( dot-node-ptr-ptr )
        @
        ( dot-node-ptr )
        dup 0= invert
    while
        ( dot-node-ptr )
        r> 1 + >r
        dot-node-next
        ( dot-node-ptr-ptr )
    repeat
    ( dot-node-ptr )
    drop
    ( )
    r>
    ( count )
    ;

begin-structure cmd-node
  field: cmd-node-axis-is-x
  field: cmd-node-val
  field: cmd-node-next
end-structure

: dump-list-cmds
    begin
        @
        dup 0= invert
    while
        ." cmd: "
        dup cmd-node-axis-is-x @ decimal .
        dup cmd-node-val @ decimal .
        cr
        cmd-node-next
    repeat
    drop
    ;

: parse-number
    0 0 2swap decimal >number rot drop ;

: skip-string
    swap drop
    dup -rot -
    -rot + swap
    ;

: skip-char
    1 - swap 1 + swap ;

: save-char
    over c@ -rot
    1 - swap 1 + swap
    ;

: isdigit
    dup '0' < if
        drop 0 exit
    then
    '9' <= if
        1 exit
    then
    0 exit
    ;

: dot-node-allocate-construct
    ( x-coord y-coord )
    dot-node allocate throw
    ( x-coord y-coord dot-node-ptr )
    rot over dot-node-x !
    ( y-coord dot-node-ptr )
    dup -rot dot-node-y !
    ( dot-node-ptr )
    0 over dot-node-next !
    ( dot-node-ptr )
    ;

: parse-line-dot
    ( list-dots line-buffer read-len )
    parse-number
    ( list-dots x-coord line-buffer read-len )
    skip-char
    ( list-dots x-coord line-buffer read-len )
    parse-number
    ( list-dots x-coord y-coord line-buffer read-len )
    2drop
    ( list-dots x-coord y-coord )
    dot-node-allocate-construct
    ( list-dots dot-node-ptr )
    dup rot !
    ( dot-node-ptr )
    dot-node-next
    ( dot-node-next-ptr )
    ;

: cmd-node-allocate-construct
    ( axis-is-x val )
    cmd-node allocate throw
    ( axis-is-x val cmd-node-ptr )
    rot over cmd-node-axis-is-x !
    ( val cmd-node-ptr )
    dup -rot cmd-node-val !
    ( cmd-node-ptr )
    0 over cmd-node-next !
    ( cmd-node-ptr )
    ;

: parse-line-cmd
    ( list-cmds line-buffer read-len )
    s" fold along " skip-string
    ( list-cmds line-buffer read-len )
    save-char
    ( list-cmds axis-char line-buffer read-len )
    rot 'x' = -rot
    ( list-cmds axis-is-x line-buffer read-len )
    skip-char
    ( list-cmds axis-is-x line-buffer read-len )
    parse-number
    ( list-cmds axis-is-x val line-buffer read-len )
    2drop
    ( list-cmds axis-is-x val )
    cmd-node-allocate-construct
    ( list-cmds cmd-node-ptr )
    dup rot !
    ( cmd-node-ptr )
    cmd-node-next
    ( cmd-node-next-ptr )
    ;

: parse-line
    ( list-dots list-cmds line-buffer read-len )
    dup 0= if
        ( list-dots list-cmds line-buffer read-len )
        2drop
        ( list-dots list-cmds )
    else
        ( list-dots list-cmds line-buffer read-len )
        over c@ isdigit if
            ( list-dots list-cmds line-buffer read-len )
            >r >r swap r> r>
            ( list-cmds list-dots line-buffer read-len )
            parse-line-dot
            ( list-cmds new-list-dots )
            swap
            ( new-list-dots list-cmds )
        else
            ( list-dots list-cmds line-buffer read-len )
            parse-line-cmd
            ( list-dots new-list-cmds )
        then
        ( new-list-dots new-list-cmds)
    then
    ( new-list-dots new-list-cmds)
    ;

: open-input-file
    r/o open-file throw
    ;

: close-input-file
    close-file throw
    ;

: alloc-line-buffer
    256 \ buffer size
    dup 2 + allot here \ alloc buffer
    swap \ ( size buffer -- buffer size )
    ;

: free-line-buffer
    2 + negate allot \ ( de-alloc buffer)
    drop \ ( buffer -- )
    ;

: unpick-4
    ( tgt a3 a2 a1 a0 val )
    swap >r
    ( tgt a3 a2 a1 val )
    swap >r
    ( tgt a3 a2 val )
    swap >r
    ( tgt a3 val )
    swap >r
    ( tgt val )
    swap drop
    ( val )
    r> r> r> r> 
    ( val a3 a2 a1 a0 )
    ;

: read-parse-input
    ( list-dots list-cmds filename,len )
    open-input-file
    ( list-dots list-cmds fd )
    alloc-line-buffer
    ( list-dots list-cmds fd line-buffer,len)
    begin
        ( list-dots list-cmds fd line-buffer,len)
        2dup
        ( list-dots list-cmds fd line-buffer,len line-buffer,len )
        4 pick
        ( list-dots list-cmds fd line-buffer,len line-buffer,len fd )
        read-line throw
        ( list-dots list-cmds fd line-buffer,len read-len flag )
    while
        ( list-dots list-cmds fd line-buffer,len read-len )
        5 pick swap
        ( list-dots list-cmds fd line-buffer,len list-dots read-len )
        5 pick swap
        ( list-dots list-cmds fd line-buffer,len list-dots list-cmds read-len )
        4 pick swap
        ( list-dots list-cmds fd line-buffer,len list-dots list-cmds line-buffer read-len )
        parse-line
        ( list-dots list-cmds fd line-buffer,len new-list-dots new-list-cmds )
        unpick-4
        ( list-dots new-list-cmds fd line-buffer,len new-list-dots )
        unpick-4
        ( new-list-dots new-list-cmds fd line-buffer,len )
    repeat
    ( list-dots list-cmds fd line-buffer,len read-len )
    drop
    ( list-dots list-cmds fd line-buffer,len )
    free-line-buffer
    ( list-dots list-cmds fd )
    close-input-file
    ( list-dots list-cmds )
    2drop
    ( )
    ;

: apply-cmd-xy
    ( dot-node-ptr val offset )
    rot + swap
    ( xy-ptr val )
    over @
    ( xy-ptr val xy )
    2dup < if
        ( xy-ptr val xy )
        \ xy = val - (xy - val)
        \ xy = val - xy + val
        \ xy = 2 * val - xy
        swap 2 * swap -
        ( xy-ptr 2*val-xy )
        swap !
        ( )
    else
        ( xy-ptr val xy )
        2drop
        ( xy-ptr )
        drop
        ( )
    then
    ( )
    ;

: apply-cmd-one-dot
    ( dot-node-ptr cmd-node-ptr )
    dup cmd-node-val @
    ( dot-node-ptr cmd-node-ptr val )
    over cmd-node-axis-is-x @
    ( dot-node-ptr cmd-node-ptr val axis-is-x )
    rot drop
    ( dot-node-ptr val axis-is-x )
    if
        ( dot-node-ptr val )
        0 dot-node-x
        ( dot-node-ptr val offet )
    else
        ( dot-node-ptr val )
        0 dot-node-y
        ( dot-node-ptr val offet )
    then
    ( dot-node-ptr val offet )
    apply-cmd-xy
    ( )
    ;

: cells-at-ptrs-offset-equal
    ( a-ptr b-ptr offset )
    dup
    ( a-ptr b-ptr offset offset )
    -rot
    ( a-ptr offset b-ptr offset )
    + @
    ( a-ptr offset b )
    -rot
    ( b a-ptr offset )
    + @
    ( b a )
    =
    ( equal )
    ;

: dot-nodes-equal
    ( node-a-ptr node-b-ptr )
    2dup
    ( node-a-ptr node-b-ptr node-a-ptr node-b-ptr )
    0 dot-node-x cells-at-ptrs-offset-equal invert
    ( node-a-ptr node-b-ptr !x-equal )
    if
        ( node-a-ptr node-b-ptr )
        2drop
        ( )
        0 exit
    then
    ( node-a-ptr node-b-ptr )
    0 dot-node-y cells-at-ptrs-offset-equal invert
    ( !y-equal )
    if
        ( )
        0 exit
    then
    ( )
    -1 exit
    ;

: find-ptr-to-equal-dot-node
    ( dot-node-ptr list-dots/test-dot-node-ptr-ptr )
    begin
        ( dot-node-ptr test-dot-node-ptr-ptr )
        over over @
        ( dot-node-ptr test-dot-node-ptr-ptr dot-node-ptr test-dot-node-ptr )
        dot-nodes-equal invert
        ( dot-node-ptr test-dot-node-ptr-ptr !equal )
     while
        ( dot-node-ptr test-dot-node-ptr-ptr )
        @ dot-node-next
        ( dot-node-ptr next-test-dot-node-ptr-ptr )
    repeat
    ( dot-node-ptr test-dot-node-ptr-ptr )
    swap drop
    ( test-dot-node-ptr-ptr )
    ;

: unlink-dot-if-duplicate
    ( dot-node-ptr list-dots/dot-node-ptr-ptr )
    over swap
    ( dot-node-ptr dot-node-ptr list-dots/dot-node-ptr-ptr )
    find-ptr-to-equal-dot-node
    ( dot-node-ptr ptr-to-equal-dot-node-ptr )
    dup @
    ( dot-node-ptr ptr-to-equal-dot-node-ptr equal-dot-node-ptr )
    rot
    ( ptr-to-equal-dot-node-ptr equal-dot-node-ptr dot-node-ptr )
    = invert if
        \ duplicate nodes; unlink one
        ( ptr-to-equal-dot-node-ptr )
        dup @
        ( ptr-to-equal-dot-node-ptr equal-dot-node-ptr )
        dot-node-next @
        ( ptr-to-equal-dot-node-ptr next-dot-node-ptr )
        swap !
        ( )
    else
        ( ptr-to-equal-dot-node-ptr )
        drop
        ( )
    then
    ( )
    ;

: apply-cmd-all-dots
    ( cmd-node-ptr list-dots/dot-node-ptr-ptr  )
    dup >r
    ( r: list-dots/dot-node-ptr-ptr)
    begin
        ( cmd-node-ptr dot-node-ptr-ptr )
        @
        ( cmd-node-ptr dot-node-ptr )
        dup 0= invert
        ( cmd-node-ptr dot-node-ptr !is-null )
    while
        ( cmd-node-ptr dot-node-ptr )
        2dup swap apply-cmd-one-dot
        ( cmd-node-ptr dot-node-ptr )
        dup r@ unlink-dot-if-duplicate
        ( cmd-node-ptr dot-node-ptr )
        dot-node-next
        ( cmd-node-ptr next-dot-node-ptr-ptr )
    repeat
    ( cmd-node-ptr dot-node-ptr )
    2drop
    ( )
    r> drop
    ( r: )
    ;

: apply-cmds
    ( list-dots list-cmds )
    @ swap apply-cmd-all-dots
    ( )
    ;

variable list-dots
variable list-cmds
list-dots list-cmds s" ../input/day13.txt" read-parse-input
\ ." pre:" cr
\ list-dots dump-list-dots
\ list-cmds dump-list-cmds
\ cr

list-dots list-cmds apply-cmds
\ ." post:" cr
\ list-dots dump-list-dots
list-dots count-list-dots . cr

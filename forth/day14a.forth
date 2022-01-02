: int-pair-byte-offset
    ( n1 n2 )
    swap 26 * +
    ( offset )
    ;

: int-pair-cell-offset
    ( n1 n2 )
    int-pair-byte-offset cells
    ( offset )
    ;

: char-pair-byte-offset
    ( ch1 ch2 )
    swap 'A' - swap
    ( n1 ch2 )
    'A' -
    ( n1 n2 )
    int-pair-byte-offset
    ( offset )
    ;

: char-pair-cell-offset
    ( ch1 ch2 )
    char-pair-byte-offset cells
    ( offset )
    ;

: open-input-file
    ( filename,len )
    r/o open-file throw
    ( fd )
    ;

: close-input-file
    ( fd )
    close-file throw
    ( )
    ;

: alloc-line-buffer
    ( )
    256
    ( len )
    dup 2 + allot
    ( len )
    here
    ( len,buffer )
    swap
    ( buffer,len )
    ;

: free-line-buffer
    ( line-buffer,len )
    2 + negate allot
    ( line-buffer )
    drop
    ;

: read-input-line
    ( fd line-buffer,len )
    2dup
    ( fd line-buffer,len line-buffer,len )
    4 pick
    ( fd line-buffer,len line-buffer,len fd )
    read-line throw
    ( fd line-buffer,len read-len flag )
    ;

: cell-accumulate
    ( val addr )
    dup @
    ( val addr old-val )
    rot +
    ( addr new-val )
    swap !
    ( )
    ;

: parse-pair-counts
    ( pair-counts line-buffer,read-len )
    begin
        ( pair-counts line-buffer,read-len )
        dup 2 < invert
        ( pair-counts line-buffer,read-len 2-or-more-chars-left )
    while
        ( pair-counts line-buffer,read-len )
        over c@
        ( pair-counts line-buffer,read-len ch1 )
        2 pick 1 + c@
        ( pair-counts line-buffer,read-len ch1 ch2 )
        char-pair-cell-offset 3 pick +
        ( pair-counts line-buffer,read-len pair-count-entry-addr )
        1 swap cell-accumulate
        ( pair-counts line-buffer,read-len )
        swap 1 + swap
        ( pair-counts next-line-buffer,read-len )
        1 -
        ( pair-counts next-line-buffer,next-read-len )
    repeat
    ( pair-counts line-buffer,read-len )
    2drop drop
    ( )
    ;

: get-template-first-last
    ( buffer len )
    over c@ 'A' -
    ( buffer len first )
    -rot + 1 - c@ 'A' -
    ( first last )
    ;

: read-parse-template
    ( pair-counts fd line-buffer,len )
    read-input-line
    ( pair-counts fd line-buffer,len read-len flag )
    invert if abort then
    ( pair-counts fd line-buffer,len read-len )
    2 pick over get-template-first-last
    ( pair-counts fd line-buffer,len read-len elem-first elem-last )
    2>r
    ( pair-counts fd line-buffer,len read-len )
    2 pick swap
    ( pair-counts fd line-buffer,len line-buffer,read-len )
    5 roll -rot
    ( fd line-buffer,len pair-counts line-buffer,read-len )
    parse-pair-counts
    ( fd line-buffer,len )
    2r>
    ( fd line-buffer,len elem-first elem-last )
    ;

: read-parse-blank-line
    ( fd line-buffer,len )
    read-input-line
    ( fd line-buffer,len read-len flag )
    invert if abort then
    ( fd line-buffer,len read-len )
    0 = invert if abort then
    ( fd line-buffer,len )
    ;

: record-rule
    ( rules-ptr l r produced )
    >r
    char-pair-byte-offset +
    r> swap c!
    ;

: parse-rule
    ( rules line-buffer )
    dup c@
    ( rules line-buffer ch1 )
    over 1 + c@
    ( rules line-buffer ch1 ch2 )
    2 pick 6 + c@
    ( rules line-buffer ch1 ch2 ch-inserted )
    'A' -
    ( rules line-buffer ch1 ch2 n-inserted )
    -rot
    ( rules line-buffer n-inserted ch1 ch2 )
    char-pair-byte-offset
    ( rules line-buffer n-inserted rules-entry-offset )
    3 pick +
    ( rules line-buffer n-inserted rules-entry-addr )
    c!
    ( rules line-buffer )
    2drop
    ( )
    ;

: read-parse-rules
    ( rules fd line-buffer,len )
    begin
        ( rules fd line-buffer,len )
        2dup
        ( rules fd line-buffer,len line-buffer,len )
        4 pick
        ( rules fd line-buffer,len line-buffer,len fd )
        read-line throw
        ( rules fd line-buffer,len read-len flag )
    while
        ( rules fd line-buffer,len read-len )
        drop
        ( rules fd line-buffer,len )
        3 pick
        ( rules fd line-buffer,len rules )
        2 pick
        ( rules fd line-buffer,len rules line-buffer  )
        parse-rule
        ( rules fd line-buffer,len )
    repeat
    ( rules fd line-buffer,len read-len )
    drop
    ( rules fd line-buffer,len )
    3 roll drop
    ( fd line-buffer,len )
    ;

: read-parse-input
    ( rules pair-counts filename,len )
    open-input-file
    ( rules pair-counts fd )
    alloc-line-buffer
    ( rules pair-counts fd line-buffer,len )
    read-parse-template
    2>r
    ( rules fd line-buffer,len )
    read-parse-blank-line
    ( rules fd line-buffer,len )
    read-parse-rules
    ( fd line-buffer,len )
    free-line-buffer
    ( fd )
    close-input-file
    ( )
    2r>
    ( elem-first elem-last )
    ;

: dump-pair-counts
    26 0 do
        i 'A' + emit ." : "
        26 0 do
            ." >" i 'A' + emit 32 emit
            j i int-pair-cell-offset
            over +
            @ .
        loop
        cr
    loop
    drop
    ;

: dump-rules
    26 0 do
        26 0 do
            j 'A' + emit
            i 'A' + emit
            ."  -> "
            j i int-pair-byte-offset
            over +
            c@ 'A' + emit
            cr
        loop
    loop
    drop
    ;

26 constant max-elements
max-elements max-elements * constant num-pairs
variable pair-counts num-pairs cells allot
variable rules num-pairs allot
variable first-element
variable last-element

pair-counts num-pairs cells 0 fill
rules num-pairs 0 fill

rules pair-counts s" ../input/day14.txt" read-parse-input
last-element !
first-element !

\ first-element @ . cr
\ last-element @ . cr

\ rules dump-rules
\ ." pair counts before step 1" cr
\ pair-counts dump-pair-counts

: pair-counts@
    ( n1 n2 )
    int-pair-cell-offset
    ( pair-count-entry-offset )
    pair-counts + @
    ( inserted-num )
    ;

: new-pair-counts@
    ( n1 n2 )
    int-pair-cell-offset
    ( pair-count-entry-offset )
    pad + @
    ( inserted-num )
    ;

: new-pair-counts!
    ( val n1 n2 )
    int-pair-cell-offset
    ( val pair-count-entry-offset )
    pad +
    ( val pair-count-entry-addr )
    !
    ( )
    ;

: new-pair-counts+=
    ( val n1 n2 )
    2dup new-pair-counts@
    ( val n1 n2 cur-val )
    3 roll
    ( n1 n2 cur-val val )
    +
    ( n1 n2 new-val )
    -rot
    ( new-val n1 n2 )
    new-pair-counts!
    ( )
    ;

: rule@
    ( n1 n2 )
    int-pair-byte-offset
    ( rules-entry-offset )
    rules + c@
    ( inserted-num )
    ;

: accumulate-new-pair-count
    ( n1 n2 val )
    -rot new-pair-counts+=
    ( )
    ;

: apply-rule
    ( n1 n2 )
    2dup pair-counts@ >r
    ( n1 n2 )
    2dup rule@
    ( n1 n2 inserted-num )
    rot over
    ( n2 inserted-num n1 inserted-num )
    r@ accumulate-new-pair-count
    ( n2 inserted-num )
    swap
    ( inserted-num n2 )
    r> accumulate-new-pair-count
    ( )
    ;

: apply-step
    pad num-pairs cells 0 fill
    26 0 do
        26 0 do
            j i apply-rule
        loop
    loop
    pad pair-counts num-pairs cells cmove
    ;

: apply-steps
    0 do
        apply-step
        \ ." pair counts after step " i 1 + . cr
        \ pair-counts dump-pair-counts
    loop
    ;

10 apply-steps

variable element-counts max-elements cells allot

: accumulate-element-counts
    ( element count-to-add )
    over cells element-counts + @
    ( element count-to-add cur-count )
    +
    ( element new-count )
    over cells element-counts + !
    ( element )
    ;

: count-elements-row
    ( n1 )
    26 0 do
        ( n1 )
        dup i pair-counts@
        ( n1 count-to-add )
        accumulate-element-counts
        ( n1 )
    loop
    ( n1 )
    drop
    ( )
    ;

: count-elements-column
    ( n2 )
    26 0 do
        ( n2 )
        i over pair-counts@
        ( n2 count-to-add )
        accumulate-element-counts
        ( n2 )
    loop
    ( n1 )
    drop
    ( )
    ;

: count-elements
    element-counts max-elements cells 0 fill
    26 0 do
        i count-elements-row
        i count-elements-column
    loop
    last-element @ 1 accumulate-element-counts drop
    first-element @ 1 accumulate-element-counts drop
    26 0 do
        ( )
        i cells element-counts +
        ( elements-entry-addr )
        dup @
        ( elements-entry-addr double-count )
        2 /
        ( elements-entry-addr count )
        swap !
        ( )
    loop
    ;

: dump-element-counts
    26 0 do
        i 'A' + emit 32 emit
        dup i cells + @ .
        cr
    loop
    ;

: max-element-count
    ( )
    0
    ( max-count )
    26 0 do
        ( max-count )
        i cells element-counts + @
        ( max-count element-count )
        2dup < if
            ( max-count element-count )
            swap drop
            ( element-count/new-max-count )
        else
            ( max-count element-count )
            drop
            ( max-count )
        then
        ( max-count)
    loop
    ( max-count )
    ;

: min-element-count
    ( )
    0x7fffffffffffffff
    ( min-count )
    26 0 do
        ( min-count )
        i cells element-counts + @
        ( min-count element-count )
        dup 0= if
            ( min-count element-count )
            drop
            ( min-count )
        else
            ( min-count element-count )
            2dup > if
                ( min-count element-count )
                swap drop
                ( element-count/new-min-count )
            else
                ( min-count element-count )
                drop
                ( min-count )
            then
            ( min-count)
        then
        ( min-count)
    loop
    ( min-count )
    ;

count-elements
\ element-counts dump-element-counts
max-element-count
min-element-count
- . cr

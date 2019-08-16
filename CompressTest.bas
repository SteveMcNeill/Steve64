text$ = "I like cheese."
FOR i = 1 TO 10: text$ = text$ + text$: NEXT
de$ = _DEFLATE$(text$)
PRINT LEN(de$), LEN(text$)
PRINT de$
SLEEP
CLS
o$ = _INFLATE$(de$, LEN(text$)) 'Note:  _INFLATE$ requires that we know the size of the original text to get it back properly.
'                                When saving your compressed data, it's probably a wise idea to save it with the legnth
'                                before the compressed information, like most archives, images, ect does.
'                                Just:
'                                PUT #filehandle, ,OriginalSize
'                                PUT #filehandle, ,CompressedString
PRINT o$
PRINT LEN(o$), LEN(text$)

// Summary: Busca um valor por chave em hash ou array de pares.
FUNCTION JsonGet( xJson, cKey )
   LOCAL n
   LOCAL xPair

   IF ValType( xJson ) == "H"
      IF HHasKey( xJson, cKey )
         RETURN xJson[ cKey ]
      ENDIF
      RETURN NIL
   ENDIF

   IF ValType( xJson ) == "A"
      FOR n := 1 TO Len( xJson )
         xPair := xJson[ n ]
         IF ValType( xPair ) == "A" .AND. Len( xPair ) >= 2 .AND. ValType( xPair[ 1 ] ) == "C"
            IF xPair[ 1 ] == cKey
               RETURN xPair[ 2 ]
            ENDIF
         ENDIF
      NEXT
   ENDIF
RETURN NIL

// Summary: Converte array de pares em hash.
FUNCTION JsonToHash( xJson )
   LOCAL hOut := {=>}
   LOCAL n
   LOCAL xPair

   IF ValType( xJson ) == "H"
      RETURN xJson
   ENDIF

   IF ValType( xJson ) == "A"
      FOR n := 1 TO Len( xJson )
         xPair := xJson[ n ]
         IF ValType( xPair ) == "A" .AND. Len( xPair ) >= 2 .AND. ValType( xPair[ 1 ] ) == "C"
            hOut[ xPair[ 1 ] ] := xPair[ 2 ]
         ENDIF
      NEXT
   ENDIF
RETURN hOut

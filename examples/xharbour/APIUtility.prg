// Summary: Extrai texto amigavel de um erro COM/Harbour.
// Params:
//   oErr -> objeto de erro
// Return: texto descritivo
FUNCTION ErrText( oErr )
   LOCAL cText := ""
   LOCAL xVal
   LOCAL bOldError

   IF oErr == NIL
      RETURN ""
   ENDIF

   bOldError := ErrorBlock( { |e| Break( e ) } )
   BEGIN SEQUENCE
      xVal := __objSendMsg( oErr, "Description" )
      IF ValType( xVal ) == "C" .AND. ! Empty( xVal )
         cText := xVal
      ENDIF

      xVal := __objSendMsg( oErr, "Operation" )
      IF ValType( xVal ) == "C" .AND. ! Empty( xVal )
         cText += IIF( Empty( cText ), "", " (" ) + xVal + IIF( Empty( cText ), "", ")" )
      ENDIF
   RECOVER
   END SEQUENCE
   ErrorBlock( bOldError )

   IF Empty( cText )
      cText := "falha de comunicacao; verifique DNS/SSL/proxy"
   ENDIF
RETURN cText

// Summary: Converte data para ISO (YYYY-MM-DD).
FUNCTION DateToIso( dDate )
   LOCAL cDate := DToS( dDate )

   IF Empty( cDate ) .OR. Len( cDate ) < 8
      RETURN ""
   ENDIF
RETURN SubStr( cDate, 1, 4 ) + "-" + SubStr( cDate, 5, 2 ) + "-" + SubStr( cDate, 7, 2 )

// Summary: Salva conteudo Base64 em disco.
// Params:
//   cBase64     -> conteudo Base64
//   cFile       -> caminho completo/relativo do arquivo destino (opcional)
//   cDir        -> pasta destino quando cFile estiver vazio
//   cPrefix     -> prefixo do arquivo quando cFile estiver vazio
//   cExt        -> extensao do arquivo (com ou sem ponto)
//   lAllowEmpty -> se .T., base64 vazio retorna "" sem erro
// Return: string de erro ou vazio em caso de sucesso
FUNCTION SaveBase64ToFileEx( cBase64, cFile, cDir, cPrefix, cExt, lAllowEmpty )
   LOCAL cBin := ""
   LOCAL nHandle := -1
   LOCAL nWritten := 0
   LOCAL cOutFile := ""
   LOCAL lCreateDir := .F.

   IF lAllowEmpty == NIL
      lAllowEmpty := .F.
   ENDIF

   IF Empty( cBase64 )
      RETURN IIF( lAllowEmpty, "", "Conteudo Base64 vazio" )
   ENDIF

   IF ValType( cFile ) == "C" .AND. ! Empty( cFile )
      cOutFile := cFile
   ELSE
      IF ValType( cDir ) <> "C" .OR. Empty( cDir )
         RETURN "Parametros invalidos para SaveBase64ToFileEx"
      ENDIF

      IF ValType( cPrefix ) <> "C" .OR. Empty( cPrefix )
         cPrefix := "File_"
      ENDIF

      IF ValType( cExt ) <> "C"
         cExt := ""
      ENDIF

      IF ! Empty( cExt ) .AND. Left( cExt, 1 ) <> "."
         cExt := "." + cExt
      ENDIF

      cOutFile := cDir + "\" + cPrefix + DToS( Date() ) + "_" + StrTran( Time(), ":", "" ) + cExt
      lCreateDir := .T.
   ENDIF

   IF lCreateDir .AND. ! IsDirectory( cDir )
      IF MakeDir( cDir ) <> 0
         RETURN "Falha ao criar pasta " + cDir
      ENDIF
   ENDIF

   cBin := hb_base64Decode( cBase64 )
   IF Empty( cBin )
      RETURN "Falha ao decodificar Base64"
   ENDIF

   nHandle := FCreate( cOutFile )
   IF nHandle < 0
      RETURN "Falha ao criar arquivo " + cOutFile
   ENDIF

   nWritten := FWrite( nHandle, cBin )
   FClose( nHandle )

   IF nWritten <> Len( cBin )
      RETURN "Falha ao gravar arquivo " + cOutFile
   ENDIF
RETURN ""
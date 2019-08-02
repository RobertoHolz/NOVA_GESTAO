       identification division.
       program-id. "CADASTRO_VENDEDOR".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-vendedor.cpy'.
         copy 'select-arq-impvnd.cpy'.
       
       data division.
       file section.
         copy 'fd-arq-vendedor.cpy'.
         copy 'fd-arq-impvnd.cpy'.
       
       working-storage section.
	       01 ws-variaveis.
		      05 wid-arq-vendedor     pic x(22) value SPACES.          
			  05 WS-RST-ACESS-VND     pic 9(02) value ZEROS.            
              05 WID-ARQ-IMPVND       PIC X(22) VALUE SPACES.           
              05 WS-RST-ACESS-IMPVND  PIC 9(02) VALUE ZEROS.            
              05 WS-ERRO-ABERTURA     PIC 9(02) VALUE ZEROS.
              05 WS-EXISTE-VENDEDOR   PIC 9(01) VALUE ZEROS.           
              05 ws-acao              pic 9(01) value zeros.
              05 WS-PARAM-OK          PIC X(02) VALUE SPACES.
              05 WS-TELA-CAD-VND.
                 15 WS-MSG-CAD-VND          PIC X(40) VALUE SPACES.     
                 15 WS-MSG-CAD-VND-R REDEFINES WS-MSG-CAD-VND.          
                    20 WS-MSG-1             PIC X(38).
                    20 WS-MSG-STATUS        PIC 9(02).
                 15 WS-MSG-CAD-VND-R1 REDEFINES WS-MSG-CAD-VND.         
                    20 WS-MSG-2             PIC X(35).
                    20 WS-NUMREG-IMPVND-MSG PIC 9(05).                  
              05 ws-dados-tela-vnd.
                 15 WS-TEL-VND-CODIGO       pic 9(003) values zeros.    
                 15 WS-TEL-VND-CPF          pic 9(011) values zeros.    
		         15 WS-TEL-VND-NOME         pic X(040) values spaces.   
		         15 WS-TEL-VND-LATITUDE     pic s9(003)v9(008)          
                    values zeros.
		         15 WS-TEL-VND-LONGITUDE    pic s9(003)v9(008)          
                    values zeros.
              05 WS-TEL-VND-CONFIRMAR       pic x(01) value spaces.     
              05 WS-TEL-NMARQUIVO           PIC X(40) VALUE SPACES.
              05 WS-NUMREG-IMPVND           PIC 9(05) VALUE ZEROS.      
              
              
       SCREEN SECTION.
       01 TELA-CAD-VENDEDOR.
          05 VALUE "--- CADASTRO DE VENDEDORES ---" BLANK SCREEN  LINE 
          1
          COL 35.
          05 VALUE "OPCAO.: " LINE 3 COL 20.
          05 ACAO-INPUT                          LINE 3 COL 28
                    PIC 9         TO ws-acao.
          05 VALUE
          "--------------------- OPCOES ------------------------"
          LINE 16 COL 20.
          05 VALUE 
          "1-INCLUIR  2-ALTERAR  3-EXCLUIR  4-IMPORTAR  9-VOLTAR"
          LINE 17 COL 20.
          05 TELA-CAD-VENDEDOR-MSG.
             10 LINE 20 COL 20 VALUE "Mensagem:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-MSG-CAD-VND.           
       
       01 TELA-IMPVND.
          05  TELA-IMPVND-NMARQ.
              10 LINE 06 COLUMN 20 VALUE "Nome do arquivo: ".
              10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-NMARQUIVO.        
             
       01 TELA-DADOS-VND-CH.
          05 TELA-DADOS-COD-CLI.
             10 LINE 06 COL 20 VALUE "Codigo do Vendedor......:".
             10 COLUMN PLUS 2 PIC Z(3) USING WS-TEL-VND-CODIGO.         
       01 TELA-DADOS-VND-CORPO.
          05 TELA-DADOS-CPF-VND.
             10 LINE 07 COL 20 VALUE "CPF do Vendedor.........:".
             10 COLUMN PLUS 2 PIC Z(11) USING WS-TEL-VND-CPF.           
          05 TELA-DADOS-NOME-VND.
             10 LINE 08 COL 20 VALUE "Nome do Vendedor........:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-VND-NOME.          
          05 TELA-DADOS-LATIT-VND.
             10 LINE 09 COL 20 VALUE "Latitude................:".
             10 COLUMN PLUS 2 PIC s9(003)v9(008) 
                USING WS-TEL-VND-LATITUDE.
          05 TELA-DADOS-LONGI-VND.
             10 LINE 10 COL 20 VALUE "Longitude...............:".
             10 COLUMN PLUS 2 PIC s9(003)v9(008) 
                USING WS-TEL-VND-LONGITUDE.
          05 TELA-DADOS-CONFIRMAR-VND.
             10 LINE 13 COL 20 VALUE "Confirmar(S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-VND-CONFIRMAR.     
       
       procedure division.
       00-controle section.
	       perform 01-inicializar.
	       perform 02-processar 
             until ws-acao = 9
                OR WS-ERRO-ABERTURA <> 0.
	       perform 03-finalizar.
           goback.
       00-controle-exit. exit.
		 
	   01-inicializar section.
	       initialize ws-variaveis.
           MOVE "ARQ_VENDEDOR" TO wid-arq-vendedor.
           OPEN I-O ARQ-VENDEDOR.
           MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS.
           IF  WS-RST-ACESS-VND <> 00
           AND WS-RST-ACESS-VND <> 05 THEN
               MOVE "ERRO ABERTURA ARQ ARQVND" TO WS-MSG-1
               MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS
               MOVE 1 TO WS-ERRO-ABERTURA
               DISPLAY TELA-CAD-VENDEDOR
               ACCEPT TELA-CAD-VENDEDOR
           END-IF.
           
       01-inicializar-exit. exit.
          
       02-processar section.
           DISPLAY TELA-CAD-VENDEDOR.
           ACCEPT TELA-CAD-VENDEDOR.
           MOVE SPACES TO WS-MSG-CAD-VND
           DISPLAY TELA-CAD-VENDEDOR-MSG.
           if ws-acao = 4 then
              PERFORM 024-IMPORTAR-VENDEDOR
           else
           if ws-acao = 9 then
              display "sair"
           else
              DISPLAY TELA-DADOS-VND-CH
              ACCEPT TELA-DADOS-COD-CLI
              perform 029-VERIFICAR-VENDEDOR
              if ws-acao = 1 then
                  IF WS-EXISTE-VENDEDOR = 0 THEN
                      perform 021-INCLUIR-VENDEDOR
                  ELSE
                      MOVE "Vendedor já existente" TO WS-MSG-CAD-VND     
                  END-IF
              else
              if ws-acao = 2 then
                  IF WS-EXISTE-VENDEDOR = 1 THEN
                      PERFORM 022-ALTERAR-VENDEDOR
                  ELSE
                      MOVE "Vendedor inexistente" TO WS-MSG-CAD-VND      
                  END-IF
              else
              if ws-acao = 3 then
                  IF WS-EXISTE-VENDEDOR = 1 THEN
                      PERFORM 023-EXCLUIR-VENDEDOR
                  ELSE
                      MOVE "Vendedor inexistente" TO WS-MSG-CAD-VND      
                  END-IF
              end-if
              end-if
              end-if
           end-if
           end-if.
       02-processar-exit. exit.
           
       021-INCLUIR-VENDEDOR section.
           DISPLAY TELA-DADOS-VND-CORPO.
           PERFORM 0211-ACEITA-DADOS.
           
          EVALUATE WS-TEL-VND-CONFIRMAR
           WHEN 'S'
             move ws-dados-tela-vnd to VND-REGISTRO
             write VND-REGISTRO
             IF  WS-RST-ACESS-VND <> 0 THEN
                 MOVE "ERRO WRITE ARQ ARQVND" TO WS-MSG-1
                 MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS                 
             ELSE
                 MOVE "Vendedor incluido com sucesso" TO 
                      WS-MSG-CAD-VND  
             END-IF
           WHEN 'N' 
             MOVE "Inclusão não realizada" TO WS-MSG-CAD-VND            
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-VND
           END-EVALUATE.	           
           
       021-INCLUIR-VENDEDOR-EXIT.
           EXIT.
  
       0211-ACEITA-DADOS SECTION.
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-CPF
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-NOME
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-LATITUDE
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-LONGITUDE
             UNTIL WS-PARAM-OK EQUAL "OK".

           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-CONFIRMAR
             UNTIL WS-PARAM-OK EQUAL "OK".
           
       0211-ACEITA-DADOS-EXIT.
           EXIT.

       02111-INFORMA-CPF SECTION.
             ACCEPT  TELA-DADOS-CPF-VND.
            IF WS-TEL-VND-CPF EQUAL 0 THEN
                MOVE "CPF invalido" TO WS-MSG-CAD-VND                   
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-VND
            END-IF.
            DISPLAY TELA-CAD-VENDEDOR-MSG.
          
       02111-INFORMA-CPF-EXIT.
           EXIT.
           
       02111-INFORMA-NOME SECTION.
            ACCEPT  TELA-DADOS-NOME-VND.
            IF WS-TEL-VND-NOME EQUAL SPACES THEN
                MOVE "Nome invalido" TO WS-MSG-CAD-VND                  
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-VND
            END-IF.
            DISPLAY TELA-CAD-VENDEDOR-MSG.
           
       02111-INFORMA-NOME-EXIT.
           EXIT.
           
       02111-INFORMA-LATITUDE SECTION.
            ACCEPT  TELA-DADOS-LATIT-VND.
            IF WS-TEL-VND-LATITUDE EQUAL 0 THEN
                MOVE "Latitude invalida" TO WS-MSG-CAD-VND              
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-VND
            END-IF.
            DISPLAY TELA-CAD-VENDEDOR-MSG.
          
       02111-INFORMA-LATITUDE-EXIT.
           EXIT.
           
       02111-INFORMA-LONGITUDE SECTION.
            ACCEPT  TELA-DADOS-LONGI-VND.
            IF WS-TEL-VND-LONGITUDE EQUAL 0 THEN
                MOVE "Longitude invalida" TO WS-MSG-CAD-VND             
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-VND
            END-IF.
            DISPLAY TELA-CAD-VENDEDOR-MSG.
          
       02111-INFORMA-LONGITUDE-EXIT.
           EXIT.
           
       02111-INFORMA-CONFIRMAR SECTION.
            ACCEPT  TELA-DADOS-CONFIRMAR-VND.
            IF  WS-TEL-VND-CONFIRMAR <> "S" 
            AND WS-TEL-VND-CONFIRMAR <> "N" THEN
                MOVE "Opcao invalida" TO WS-MSG-CAD-VND                 
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-VND
            END-IF.
            DISPLAY TELA-CAD-VENDEDOR-MSG.
          
       02111-INFORMA-CONFIRMAR-EXIT.
           EXIT.

       022-ALTERAR-VENDEDOR section.
           MOVE VND-CODIGO         TO WS-TEL-VND-CODIGO.
           MOVE VND-CPF            TO WS-TEL-VND-CPF.
           MOVE VND-NOME           TO WS-TEL-VND-NOME.
           MOVE VND-LATITUDE       TO WS-TEL-VND-LATITUDE.
           MOVE VND-LONGITUDE      TO WS-TEL-VND-LONGITUDE.
           
           DISPLAY TELA-DADOS-VND-CORPO.
           PERFORM 0211-ACEITA-DADOS.
           
           EVALUATE WS-TEL-VND-CONFIRMAR
           WHEN 'S'
             move ws-dados-tela-vnd to VND-REGISTRO
             REWRITE VND-REGISTRO
             IF  WS-RST-ACESS-VND <> 0 THEN
                 MOVE "ERRO REWRITE ARQ ARQVND" TO WS-MSG-1
                 MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS                 
             ELSE
                 MOVE "Vendedor alterado com sucesso" TO                 
                      WS-MSG-CAD-VND  
             END-IF
           WHEN 'N' 
             MOVE "Alteracao nao realizada" TO WS-MSG-CAD-VND           
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-VND
           END-EVALUATE.	           
        022-ALTERAR-VENDEDOR-exit. exit.
           
           
       023-EXCLUIR-VENDEDOR section.
           MOVE VND-CODIGO         TO WS-TEL-VND-CODIGO.
           MOVE VND-CPF            TO WS-TEL-VND-CPF.
           MOVE VND-NOME           TO WS-TEL-VND-NOME.
           MOVE VND-LATITUDE       TO WS-TEL-VND-LATITUDE.
           MOVE VND-LONGITUDE      TO WS-TEL-VND-LONGITUDE.             
           DISPLAY TELA-DADOS-VND-CORPO.
           
           ACCEPT TELA-DADOS-CONFIRMAR-VND.
           
           EVALUATE WS-TEL-VND-CONFIRMAR
           WHEN 'S'
             DELETE ARQ-VENDEDOR
             IF  WS-RST-ACESS-VND <> 0 THEN
                 MOVE "ERRO REWRITE ARQ ARQVND" TO WS-MSG-1
                 MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS                 
             ELSE
                 MOVE "Vendedor EXCLUIDO com sucesso" TO                 
                      WS-MSG-CAD-VND  
             END-IF
           WHEN 'N' 
             MOVE "Exclusao nao realizada" TO WS-MSG-CAD-VND            
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-VND
           END-EVALUATE.	           
           MOVE SPACES TO ws-dados-tela-vnd.
           
       023-EXCLUIR-VENDEDOR-EXIT.
           EXIT.
           
       024-IMPORTAR-VENDEDOR SECTION.
           DISPLAY TELA-IMPVND.
           ACCEPT  TELA-IMPVND-NMARQ.
           MOVE WS-TEL-NMARQUIVO TO WID-ARQ-IMPVND.
            
           OPEN INPUT ARQ-IMPVND.
           IF WS-RST-ACESS-IMPVND = 0 THEN
               MOVE 0 TO WS-NUMREG-IMPVND
               MOVE " " TO WS-MSG-1
               PERFORM 0249-LER-IMPCLI
               PERFORM 0241-PROCESSAR-IMPORTACAO
                 UNTIL WS-RST-ACESS-IMPVND > 0
                    OR WS-RST-ACESS-VND > 0
           ELSE
               MOVE "Arquivo de imp. inexistente." TO WS-MSG-1
               MOVE WS-RST-ACESS-IMPVND TO WS-MSG-STATUS                
           END-IF.
            
           CLOSE ARQ-IMPVND.
            
           IF WS-MSG-1 = " " THEN
               MOVE "Quantidade de reg. importados:" TO WS-MSG-2
               MOVE WS-NUMREG-IMPVND TO WS-NUMREG-IMPVND-MSG            
           END-IF.
            
       024-IMPORTAR-VENDEDOR-EXIT.
           EXIT.
           
       0241-PROCESSAR-IMPORTACAO SECTION.
           MOVE IMPVND-CODVND      TO VND-CODIGO.
           MOVE IMPVND-CPF         TO VND-CPF.
           MOVE IMPVND-NOME        TO VND-NOME.
           MOVE IMPVND-LATITUDE    TO VND-LATITUDE.
           MOVE IMPVND-LONGITUDE   TO VND-LONGITUDE.

           WRITE VND-REGISTRO
           IF  WS-RST-ACESS-VND <> 0 THEN
               MOVE "ERRO WRITE ARQ ARQVND" TO WS-MSG-1
               MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS
           ELSE
               ADD 1 TO WS-NUMREG-IMPVND
               PERFORM 0249-LER-IMPCLI
           END-IF.
       0241-PROCESSAR-IMPORTACAO-EXIT.
           EXIT.
           
       0249-LER-IMPCLI SECTION.
           READ ARQ-IMPVND NEXT RECORD.
       0249-LER-IMPCLI-EXIT.
           EXIT.
 
       029-VERIFICAR-VENDEDOR SECTION.
           MOVE WS-TEL-VND-CODIGO TO VND-CODIGO.
           READ ARQ-VENDEDOR KEY IS VND-CODIGO INVALID KEY.
           IF WS-RST-ACESS-VND = 0 THEN
               MOVE 1 TO WS-EXISTE-VENDEDOR
           ELSE
               MOVE 0 TO WS-EXISTE-VENDEDOR
           END-IF.
       029-VERIFICAR-VENDEDOR-EXIT.
           EXIT.
       
       03-finalizar section.
           close ARQ-VENDEDOR.
       03-finalizar-exit.
           EXIT.




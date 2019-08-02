       identification division.
       program-id. "CADASTRO_CLIENTE".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-cliente.cpy'.
         copy 'select-arq-impcli.cpy'.
       
       data division.
       file section.
         copy 'fd-arq-cliente.cpy'.
         copy 'fd-arq-impcli.cpy'.
       
       working-storage section.
	       01 ws-variaveis.
		      05 wid-arq-cliente      pic x(22) value SPACES.
			  05 WS-RESULTADO-ACESSO  pic 9(02) value ZEROS.
              05 WID-ARQ-IMPCLI       PIC X(22) VALUE SPACES.           
              05 WS-RST-ACESS-IMPCLI  PIC 9(02) VALUE ZEROS.
              05 WS-ERRO-ABERTURA     PIC 9(02) VALUE ZEROS.
              05 WS-EXISTE-CLIENTE    PIC 9(01) VALUE ZEROS.
              05 ws-acao              pic 9(01) value zeros.
              05 WS-PARAM-OK          PIC X(02) VALUE SPACES.
              05 WS-TELA-CAD-CLIENTE.
                 15 WS-MSG-CAD-CLIENTE      PIC X(40) VALUE SPACES.
                 15 WS-MSG-CAD-CLIENTE-R REDEFINES WS-MSG-CAD-CLIENTE.
                    20 WS-MSG-1             PIC X(38).
                    20 WS-MSG-STATUS        PIC 9(02).
                 15 WS-MSG-CAD-CLIENTE-R1 REDEFINES WS-MSG-CAD-CLIENTE.
                    20 WS-MSG-2             PIC X(35).
                    20 WS-NUMREG-IMPCLI-MSG PIC 9(05).
              05 ws-dados-tela-cliente.
                 15 WS-TEL-CLI-CODIGO       pic 9(007) values zeros.    
                 15 WS-TEL-CLI-CNPJ         pic 9(014) values zeros.    
		         15 WS-TEL-CLI-RAZAO        pic X(040) values spaces.   
		         15 ws-tel-CLI-LATITUDE     pic s9(003)v9(008)          
                    values zeros.
		         15 ws-tel-CLI-LONGITUDE    pic s9(003)v9(008)          
                    values zeros.
              05 WS-TEL-CLI-CONFIRMAR       pic x(01) value spaces.
              05 WS-TEL-NMARQUIVO           PIC X(40) VALUE SPACES.
              05 WS-NUMREG-IMPCLI           PIC 9(05) VALUE ZEROS.
              
              
       SCREEN SECTION.
       01 TELA-CAD-CLIENTE.
          05 VALUE "--- CADASTRO DE CLIENTES ---" BLANK SCREEN  LINE 
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
          05 TELA-CAD-CLIENTE-MSG.
             10 LINE 20 COL 20 VALUE "Mensagem:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-MSG-CAD-CLIENTE.
       
       01 TELA-IMPCLI.
          05  TELA-IMPCLI-NMARQ.
              10 LINE 06 COLUMN 20 VALUE "Nome do arquivo: ".
              10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-NMARQUIVO.        
             
       01 TELA-DADOS-CLIENTE-CH.
          05 TELA-DADOS-COD-CLI.
             10 LINE 06 COL 20 VALUE "Codigo do Cliente.......:".
             10 COLUMN PLUS 2 PIC Z(7) USING WS-TEL-CLI-CODIGO.
       01 TELA-DADOS-CLIENTE-CORPO.
          05 TELA-DADOS-CNPJ-CLI.
             10 LINE 07 COL 20 VALUE "CNPJ do Cliente.........:".
             10 COLUMN PLUS 2 PIC Z(14) USING WS-TEL-CLI-CNPJ.
          05 TELA-DADOS-RAZAO-CLI.
             10 LINE 08 COL 20 VALUE "Razao Social do Cliente.:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-CLI-RAZAO.
          05 TELA-DADOS-LATIT-CLI.
             10 LINE 09 COL 20 VALUE "Latitude................:".
             10 COLUMN PLUS 2 PIC s9(003)v9(008) 
                USING WS-TEL-CLI-LATITUDE.
          05 TELA-DADOS-LONGI-CLI.
             10 LINE 10 COL 20 VALUE "Longitude...............:".
             10 COLUMN PLUS 2 PIC s9(003)v9(008) 
                USING WS-TEL-CLI-LONGITUDE.
          05 TELA-DADOS-CONFIRMAR-CLI.
             10 LINE 13 COL 20 VALUE "Confirmar(S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-CLI-CONFIRMAR.
       
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
           MOVE "ARQ_CLIENTE" TO wid-arq-cliente.
           OPEN I-O ARQ-CLIENTE.
           MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS.
           IF  WS-RESULTADO-ACESSO <> 00
           AND WS-RESULTADO-ACESSO <> 05 THEN
               MOVE "ERRO ABERTURA ARQ ARQCLI" TO WS-MSG-1
               MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
               MOVE 1 TO WS-ERRO-ABERTURA
               DISPLAY TELA-CAD-CLIENTE
               ACCEPT TELA-CAD-CLIENTE
           END-IF.
           
       01-inicializar-exit. exit.
          
       02-processar section.
           DISPLAY TELA-CAD-CLIENTE.
           ACCEPT TELA-CAD-CLIENTE.
           MOVE SPACES TO WS-MSG-CAD-CLIENTE
           DISPLAY TELA-CAD-CLIENTE-MSG.
           if ws-acao = 4 then
              PERFORM 024-IMPORTAR-CLIENTE
           else
           if ws-acao = 9 then
              display "sair"
           else
              DISPLAY TELA-DADOS-CLIENTE-CH
              ACCEPT TELA-DADOS-COD-CLI
              perform 029-VERIFICAR-CLIENTE
              if ws-acao = 1 then
                  IF WS-EXISTE-CLIENTE = 0 THEN
                      perform 021-INCLUIR-CLIENTE
                  ELSE
                      MOVE "Cliente já existente" TO WS-MSG-CAD-CLIENTE
                  END-IF
              else
              if ws-acao = 2 then
                  IF WS-EXISTE-CLIENTE = 1 THEN
                      PERFORM 022-ALTERAR-CLIENTE
                  ELSE
                      MOVE "Cliente inexistente" TO WS-MSG-CAD-CLIENTE
                  END-IF
              else
              if ws-acao = 3 then
                  IF WS-EXISTE-CLIENTE = 1 THEN
                      PERFORM 023-EXCLUIR-CLIENTE
                  ELSE
                      MOVE "Cliente inexistente" TO WS-MSG-CAD-CLIENTE
                  END-IF
              end-if
              end-if
              end-if
           end-if
           end-if.
       02-processar-exit. exit.
           
       021-INCLUIR-CLIENTE section.
           DISPLAY TELA-DADOS-CLIENTE-CORPO.
           PERFORM 0211-ACEITA-DADOS.
           
           EVALUATE WS-TEL-CLI-CONFIRMAR
           WHEN 'S'
             move ws-dados-tela-cliente to CLI-REGISTRO
             write CLI-REGISTRO
             IF  WS-RESULTADO-ACESSO <> 0 THEN
                 MOVE "ERRO WRITE ARQ ARQCLI" TO WS-MSG-1
                 MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
             ELSE
                 MOVE "Cliente incluido com sucesso" TO 
                      WS-MSG-CAD-CLIENTE  
             END-IF
           WHEN 'N' 
             MOVE "Inclusão não realizada" TO WS-MSG-CAD-CLIENTE        
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-CLIENTE
           END-EVALUATE.	           
           
       021-INCLUIR-CLIENTE-EXIT.
           EXIT.
       
       0211-ACEITA-DADOS SECTION.
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-CNPJ
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 02111-INFORMA-RAZAO
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
         
       02111-INFORMA-CNPJ SECTION.
             ACCEPT  TELA-DADOS-CNPJ-CLI.
            IF WS-TEL-CLI-CNPJ EQUAL 0 THEN
                MOVE "CNPJ invalido" TO WS-MSG-CAD-CLIENTE      
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-CLIENTE
            END-IF.
            DISPLAY TELA-CAD-CLIENTE-MSG.
          
       02111-INFORMA-CNPJ-EXIT.
           EXIT.
           
       02111-INFORMA-RAZAO SECTION.
            ACCEPT  TELA-DADOS-RAZAO-CLI.
            IF WS-TEL-CLI-RAZAO EQUAL SPACES THEN
                MOVE "Razao Social invalida" TO WS-MSG-CAD-CLIENTE      
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-CLIENTE
            END-IF.
            DISPLAY TELA-CAD-CLIENTE-MSG.
           
       02111-INFORMA-RAZAO-EXIT.
           EXIT.
           
       02111-INFORMA-LATITUDE SECTION.
            ACCEPT  TELA-DADOS-LATIT-CLI.
            IF WS-TEL-CLI-LATITUDE EQUAL 0 THEN
                MOVE "Latitude invalida" TO WS-MSG-CAD-CLIENTE      
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-CLIENTE
            END-IF.
            DISPLAY TELA-CAD-CLIENTE-MSG.
          
       02111-INFORMA-LATITUDE-EXIT.
           EXIT.
           
       02111-INFORMA-LONGITUDE SECTION.
            ACCEPT  TELA-DADOS-LONGI-CLI.
            IF WS-TEL-CLI-LONGITUDE EQUAL 0 THEN
                MOVE "Longitude invalida" TO WS-MSG-CAD-CLIENTE      
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-CLIENTE
            END-IF.
            DISPLAY TELA-CAD-CLIENTE-MSG.
          
       02111-INFORMA-LONGITUDE-EXIT.
           EXIT.
           
       02111-INFORMA-CONFIRMAR SECTION.
            ACCEPT  TELA-DADOS-CONFIRMAR-CLI.
            IF  WS-TEL-CLI-CONFIRMAR <> "S" 
            AND WS-TEL-CLI-CONFIRMAR <> "N" THEN
                MOVE "Opcao invalida" TO WS-MSG-CAD-CLIENTE      
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CAD-CLIENTE
            END-IF.
            DISPLAY TELA-CAD-CLIENTE-MSG.
          
       02111-INFORMA-CONFIRMAR-EXIT.
           EXIT.
            
           
       022-ALTERAR-CLIENTE section.
           MOVE CLI-CODIGO TO WS-TEL-CLI-CODIGO.
           MOVE CLI-CNPJ   TO WS-TEL-CLI-CNPJ.
           MOVE CLI-RAZAOSOCIAL TO WS-TEL-CLI-RAZAO.
           MOVE CLI-LATITUDE TO WS-TEL-CLI-LATITUDE.
           MOVE CLI-LONGITUDE TO WS-TEL-CLI-LONGITUDE.
           
           DISPLAY TELA-DADOS-CLIENTE-CORPO.
           PERFORM 0211-ACEITA-DADOS.
           
           EVALUATE WS-TEL-CLI-CONFIRMAR
           WHEN 'S'
             move ws-dados-tela-cliente to CLI-REGISTRO
             REWRITE CLI-REGISTRO
             IF  WS-RESULTADO-ACESSO <> 0 THEN
                 MOVE "ERRO REWRITE ARQ ARQCLI" TO WS-MSG-1
                 MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
             ELSE
                 MOVE "Cliente alterado com sucesso" TO                 
                      WS-MSG-CAD-CLIENTE  
             END-IF
           WHEN 'N' 
             MOVE "Alteracao nao realizada" TO WS-MSG-CAD-CLIENTE       
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-CLIENTE
           END-EVALUATE.	           
        022-ALTERAR-CLIENTE-exit. exit.
           
           
       023-EXCLUIR-CLIENTE section.
           MOVE CLI-CODIGO TO WS-TEL-CLI-CODIGO.
           MOVE CLI-CNPJ   TO WS-TEL-CLI-CNPJ.
           MOVE CLI-RAZAOSOCIAL TO WS-TEL-CLI-RAZAO.
           MOVE CLI-LATITUDE TO WS-TEL-CLI-LATITUDE.
           MOVE CLI-LONGITUDE TO WS-TEL-CLI-LONGITUDE.
           DISPLAY TELA-DADOS-CLIENTE-CORPO.
           ACCEPT TELA-DADOS-CONFIRMAR-CLI.
           
           EVALUATE WS-TEL-CLI-CONFIRMAR
           WHEN 'S'
             DELETE ARQ-CLIENTE
             IF  WS-RESULTADO-ACESSO <> 0 THEN
                 MOVE "ERRO REWRITE ARQ ARQCLI" TO WS-MSG-1
                 MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
             ELSE
                 MOVE "Cliente EXCLUIDO com sucesso" TO                 
                      WS-MSG-CAD-CLIENTE  
             END-IF
           WHEN 'N' 
             MOVE "Exclusao nao realizada" TO WS-MSG-CAD-CLIENTE        
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-CAD-CLIENTE
           END-EVALUATE.	           
           MOVE SPACES TO ws-dados-tela-cliente.
           
       023-EXCLUIR-CLIENTE-EXIT.
           EXIT.
           
       024-IMPORTAR-CLIENTE SECTION.
           DISPLAY TELA-IMPCLI.
           ACCEPT  TELA-IMPCLI-NMARQ.
           MOVE WS-TEL-NMARQUIVO TO WID-ARQ-IMPCLI.
            
           OPEN INPUT ARQ-IMPCLI.
           IF WS-RST-ACESS-IMPCLI = 0 THEN
               MOVE 0 TO WS-NUMREG-IMPCLI
               MOVE " " TO WS-MSG-1
               PERFORM 0249-LER-IMPCLI
               PERFORM 0241-PROCESSAR-IMPORTACAO
                 UNTIL WS-RST-ACESS-IMPCLI > 0
                    OR WS-RESULTADO-ACESSO > 0
           ELSE
               MOVE "Arquivo de imp. inexistente." TO WS-MSG-1
               MOVE WS-RST-ACESS-IMPCLI TO WS-MSG-STATUS
           END-IF.
            
           CLOSE ARQ-IMPCLI.
            
           IF WS-MSG-1 = " " THEN
               MOVE "Quantidade de reg. importados:" TO WS-MSG-2
               MOVE WS-NUMREG-IMPCLI TO WS-NUMREG-IMPCLI-MSG
           END-IF.
            
       024-IMPORTAR-CLIENTE-EXIT.
           EXIT.
           
       0241-PROCESSAR-IMPORTACAO SECTION.
           MOVE IMPCLI-CODCLI      TO CLI-CODIGO.
           MOVE IMPCLI-CNPJ        TO CLI-CNPJ.
           MOVE IMPCLI-RAZAOSOCIAL TO CLI-RAZAOSOCIAL.
           MOVE IMPCLI-LATITUDE    TO CLI-LATITUDE.
           MOVE IMPCLI-LONGITUDE   TO CLI-LONGITUDE.

           WRITE CLI-REGISTRO
           IF  WS-RESULTADO-ACESSO <> 0 THEN
               MOVE "ERRO WRITE ARQ ARQCLI" TO WS-MSG-1
               MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
           ELSE
               ADD 1 TO WS-NUMREG-IMPCLI
               PERFORM 0249-LER-IMPCLI
           END-IF.
       0241-PROCESSAR-IMPORTACAO-EXIT.
           EXIT.
           
       0249-LER-IMPCLI SECTION.
           READ ARQ-IMPCLI NEXT RECORD.
       0249-LER-IMPCLI-EXIT.
           EXIT.
 
       029-VERIFICAR-CLIENTE SECTION.
           MOVE WS-TEL-CLI-CODIGO TO CLI-CODIGO.
           READ ARQ-CLIENTE KEY IS CLI-CODIGO INVALID KEY.
           IF WS-RESULTADO-ACESSO = 0 THEN
               MOVE 1 TO WS-EXISTE-CLIENTE
           ELSE
               MOVE 0 TO WS-EXISTE-CLIENTE
           END-IF.
       029-VERIFICAR-CLIENTE-EXIT.
           EXIT.
       
       03-finalizar section.
           close arq-cliente.
       03-finalizar-exit.
           EXIT.



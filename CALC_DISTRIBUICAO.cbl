       identification division.
       program-id. "CALC_DISTRIBUICAO".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-cliente.cpy'.
         copy 'select-arq-vendedor.cpy'.
       
       data division.
       file section.
         copy 'fd-arq-cliente.cpy'.
         copy 'fd-arq-vendedor.cpy'.
       
       working-storage section.
	       01 ws-variaveis.
		      05 wid-arq-cliente      pic x(22) value SPACES.
		      05 wid-arq-vendedor     pic x(22) value SPACES.          
              05 WS-RESULTADO-ACESSO  pic 9(02) value ZEROS.
			  05 WS-RST-ACESS-VND     pic 9(02) value ZEROS. 
              
              05 WS-CODVND-MAIS-PERTO PIC 9(03) VALUE ZEROS.
              05 WS-DISTANCIA-MENOR   PIC 9(08)V9(03) 
                                                VALUE ZEROS.
              05 WS-DISTANCIA-CALC    PIC 9(08)V9(03)
                                                VALUE ZEROS.
              05 WS-QTDREG-CLI-RW     PIC 9(07) VALUE ZEROS.                      
              05 WID-ARQ-IMPCLI       PIC X(22) VALUE SPACES.           
              05 WS-RST-ACESS-IMPCLI  PIC 9(02) VALUE ZEROS.
              05 WS-ERRO-ABERTURA     PIC 9(02) VALUE ZEROS.
              05 WS-EXISTE-CLIENTE    PIC 9(01) VALUE ZEROS.
              05 ws-acao              pic 9(01) value zeros.
              05 WS-SAIR              PIC X(01) VALUE SPACE.
              05 WS-PARAM-OK          PIC X(02) VALUE SPACES.
              05 WS-TELA-DISTRIB.
                 15 WS-MSG-DISTRIB      PIC X(40) VALUE SPACES.         
                 15 WS-MSG-DISTRIB-R REDEFINES WS-MSG-DISTRIB.          
                    20 WS-MSG-1             PIC X(38).
                    20 WS-MSG-STATUS        PIC 9(02).
                 15 WS-MSG-DISTRIB-R1 REDEFINES WS-MSG-DISTRIB.         
                    20 WS-MSG-2             PIC X(35).
                    20 WS-NUMREG-IMPCLI-MSG PIC 9(05).
                 15 WS-MSG-DISTRIB-R2 REDEFINES WS-MSG-DISTRIB.
                    20 WS-MSG-3             PIC X(25).
                    20 WS-QUALCLI-3         PIC Z(07).
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
       01 TELA-DISTRIB.
          05 VALUE "--- CALCULO DA DISTRIBUICAO ---" BLANK SCREEN  LINE 
          1
          COL 35.
          05 VALUE "OPCAO.: " LINE 3 COL 20.
          05 ACAO-INPUT                          LINE 3 COL 28
                    PIC 9         TO ws-acao.
          05 VALUE "Programa tem por objetivo alocar o vendedor" 
             LINE 6 COL 20.
          05 VALUE "mais proximo para cada cliente." LINE 7 COL 20.
          05 VALUE
          "--------------------- OPCOES ------------------------"
          LINE 16 COL 20.
          05 VALUE 
          "1-EXECUTAR  9-VOLTAR"
          LINE 17 COL 20.
          05 TELA-DISTRIB-MSG.
             10 LINE 20 COL 20 VALUE "Mensagem:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-MSG-DISTRIB.  
       01 TELA-DISTRIB-SAIR.
          05 LINE 10 COL 20 VALUE
             "Processo encerrado. Aperte qualquer tecla para voltar".
          05 COLUMN PLUS 2 PIC X(01) USING WS-SAIR.
       
       
       
       procedure division.
       00-CONTROLE SECTION.
           
           INITIALIZE ws-variaveis.
	       PERFORM 01-PEGAR-ACAO
             UNTIL ws-acao = 1
                OR ws-acao = 9.
           
           PERFORM 02-PROCESSAR.
           PERFORM 03-MOSTRAR-MENSAGEM.
           PERFORM 04-GERAR-ARQUIVO-CSV.
           
           goback.
       00-CONTROLE-exit. exit.
       
       01-PEGAR-ACAO SECTION.
           DISPLAY TELA-DISTRIB.
           ACCEPT  ACAO-INPUT.
           IF  ws-acao <> 1
           AND ws-acao <> 9 THEN
               MOVE "Opcao invalida" TO WS-MSG-DISTRIB
               DISPLAY TELA-DISTRIB-MSG
           END-IF.
       01-PEGAR-ACAO-EXIT.
           EXIT.
           
       02-PROCESSAR SECTION.
           PERFORM 029-ABRIR-ARQUIVO-CLIENTE.

           IF WS-RESULTADO-ACESSO = 0 THEN
               READ ARQ-CLIENTE NEXT RECORD
               PERFORM 021-PROCESSAR-CLIENTE
                 UNTIL WS-RESULTADO-ACESSO > 0
                    OR WS-RST-ACESS-VND > 0
               CLOSE ARQ-CLIENTE
           END-IF.
           
       02-PROCESSAR-EXIT.
           EXIT.

       021-PROCESSAR-CLIENTE SECTION.
           
           MOVE "Processando cliente:" to WS-MSG-3.
           MOVE CLI-CODIGO TO WS-QUALCLI-3.
           DISPLAY TELA-DISTRIB-MSG. 
           
           MOVE 0 TO WS-CODVND-MAIS-PERTO.
           MOVE 99999999.999 TO WS-DISTANCIA-MENOR.
           PERFORM 029-ABRIR-ARQUIVO-VENDEDOR.
           
           IF WS-RST-ACESS-VND = 0 THEN
              READ ARQ-VENDEDOR NEXT RECORD
              PERFORM 0211-PROCESSAR-VENDEDOR
                UNTIL WS-RST-ACESS-VND > 0
              PERFORM 0212-GRAVAR-VND-MAIS-PERTO
              CLOSE ARQ-VENDEDOR
              READ ARQ-CLIENTE NEXT RECORD
           END-IF.
           
       021-PROCESSAR-CLIENTE-EXIT.
           EXIT.
       
       0211-PROCESSAR-VENDEDOR SECTION.
           
      *    CHAMA SUBROTINA PARA CALCULO DA DIST ENTRE CLI E VEND
      *    AINDA SERÁ IMPLEMENTADA.
           COMPUTE WS-DISTANCIA-CALC = VND-LATITUDE.
           
           IF WS-DISTANCIA-MENOR > WS-DISTANCIA-CALC
              MOVE WS-DISTANCIA-CALC TO WS-DISTANCIA-MENOR
              MOVE VND-CODIGO TO WS-CODVND-MAIS-PERTO
           END-IF.
           
           READ ARQ-VENDEDOR NEXT RECORD.
           
       0211-PROCESSAR-VENDEDOR-EXIT.
           EXIT.
       
       0212-GRAVAR-VND-MAIS-PERTO SECTION.
           MOVE WS-CODVND-MAIS-PERTO TO CLI-CODVND.
           REWRITE CLI-REGISTRO.
           ADD 1 TO WS-QTDREG-CLI-RW.
       0212-GRAVAR-VND-MAIS-PERTO-EXIT.
           EXIT.
           
       029-ABRIR-ARQUIVO-CLIENTE SECTION.
           MOVE "ARQ_CLIENTE" TO wid-arq-cliente.
           OPEN I-O ARQ-CLIENTE.
           MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS.
           IF  WS-RESULTADO-ACESSO <> 00
           AND WS-RESULTADO-ACESSO <> 05 THEN
               MOVE "ERRO ABERTURA ARQ ARQCLI" TO WS-MSG-1
               MOVE WS-RESULTADO-ACESSO TO WS-MSG-STATUS
               MOVE 1 TO WS-ERRO-ABERTURA
               DISPLAY TELA-DISTRIB-MSG
           END-IF.

       029-ABRIR-ARQUIVO-CLIENTE-EXIT.
           EXIT.
           
       029-ABRIR-ARQUIVO-VENDEDOR SECTION.
           MOVE "ARQ_VENDEDOR" TO wid-arq-vendedor.
           OPEN INPUT ARQ-VENDEDOR.
           MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS.
           IF  WS-RST-ACESS-VND <> 00
           AND WS-RST-ACESS-VND <> 05 THEN
               MOVE "ERRO ABERTURA ARQ ARQCLI" TO WS-MSG-1
               MOVE WS-RST-ACESS-VND TO WS-MSG-STATUS
               MOVE 1 TO WS-ERRO-ABERTURA
               DISPLAY TELA-DISTRIB-MSG
           END-IF.
          
       029-029-ABRIR-ARQUIVO-VENDEDOR-EXIT.
           EXIT.
           
       03-MOSTRAR-MENSAGEM SECTION.
           MOVE SPACES TO WS-MSG-DISTRIB.
           MOVE "Qtd cliente atualizados:" TO WS-MSG-3.
           MOVE WS-QTDREG-CLI-RW TO WS-QUALCLI-3.
           DISPLAY TELA-DISTRIB.
           DISPLAY TELA-DISTRIB-SAIR.
           ACCEPT WS-SAIR.
       03-MOSTRAR-MENSAGEM-EXIT.
           EXIT.
           
       04-GERAR-ARQUIVO-CSV SECTION.
       04-04-GERAR-ARQUIVO-CSV-EXIT.
           EXIT.

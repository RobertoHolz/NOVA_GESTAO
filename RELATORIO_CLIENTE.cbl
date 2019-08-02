       identification division.
       program-id. "RELATORIO_CLIENTE".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-cliente.cpy'.
       
       data division.
       file section.
         copy 'fd-arq-cliente.cpy'.
       
       working-storage section.
	       01 ws-variaveis.
		      05 wid-arq-cliente           pic x(22) value SPACES.      
			  05 WS-RESULTADO-ACESSO       pic 9(02) value ZEROS.       
          
              05 WS-ERRO-ABERTURA     PIC 9(02) VALUE ZEROS.
              05 WS-EXISTE-VENDEDOR   PIC 9(01) VALUE ZEROS.           
              05 ws-acao              pic 9(01) value zeros.
              05 WS-SAIR              PIC 9(01) VALUE ZEROS.
              05 WS-PARAM-OK          PIC X(02) VALUE SPACES.
              05 WS-DADOS-TELA.
                 10 WS-MSG-CLI             PIC X(40) VALUE SPACES.      
                 10 WS-TEL-NMARQUIVO       PIC X(22) VALUE SPACES.
                 10 WS-TEL-PRM-ORD         PIC X(01) VALUE SPACES.
                 10 WS-TEL-PRM-CLA         PIC X(01) VALUE SPACES.
                 10 WS-TEL-PRM-CDCLI       PIC 9(07) VALUES ZEROS.      
                 10 WS-TEL-PRM-RAZAO       PIC X(40) VALUE SPACES.
                 10 WS-TEL-PRM-CDVND       PIC 9(03) VALUE ZEROS.
                 10 WS-TEL-VND-CONFIRMAR   PIC X(01) VALUE SPACES.
                 10 WS-TEL-SAIR            PIC X(01) VALUE SPACES.
             
       LINKAGE SECTION.
          copy 'lk-gerar-rel-cliente.cpy'.

       SCREEN SECTION.
       01 TELA-RELCLI.
          05 VALUE "--- GERAR RELATORIO DE CLIENTES ---" BLANK SCREEN 
          LINE 1 COL 35.
          05 TELA-RELCLI-MSG.
             10 LINE 22 COL 20 VALUE "Mensagem:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-MSG-CLI.               
       

       01 TELA-RELCLI-PRM.
         05  TELA-RELCLI-NMARQ.
             10 LINE 06 COLUMN 20 VALUE "Arquivo a gerar: ".
             10 COLUMN PLUS 2 PIC X(22) USING WS-TEL-NMARQUIVO.
          05 TELA-RELCLI-PRM-ORD.
             10 LINE 08 COL 20 VALUE "Ordenacao (A / D).......:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-PRM-ORD.
          05 TELA-RELCLI-PRM-CLA.
             10 LINE 10 COL 20 VALUE "Tipo Classificacao......:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-PRM-CLA.   
             10 LINE 11 COL 20 VALUE "  C - Codigo do Cliente".
             10 LINE 12 COL 20 VALUE "  R - Razao  do Cliente".
          05 TELA-RELCLI-PRM-FLT-CODIGO.
             10 LINE 14 COL 20 VALUE "-- FILTROS --".
             10 LINE 15 COL 20 VALUE "Codigo do Cliente.......:".
             10 COLUMN PLUS 2 PIC 9(07) USING WS-TEL-PRM-CDCLI.         
          05 TELA-RELCLI-PRM-FLT-NOME.
             10 LINE 16 COL 20 VALUE "Razao do Cliente........:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-PRM-RAZAO.  
          05 TELA-RELCLI-PRM-FLT-CODVND.
             10 LINE 17 COL 20 VALUE "Codigo do Vendedor......:".
             10 COLUMN PLUS 2 PIC 9(03) USING WS-TEL-PRM-CDVND.
          05 TELA-DADOS-CONFIRMAR-CLI.
             10 LINE 19 COL 20 VALUE "Confirmar(S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-VND-CONFIRMAR.
       01 TELA-SAIR.
          05 TELA-DADOS-SAIR.
             10 LINE 19 COL 20 VALUE "Deseja Sair (S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-SAIR.
             
         
       PROCEDURE DIVISION.
       00-CONTROLE section.
           DISPLAY TELA-RELCLI.
           DISPLAY TELA-RELCLI-PRM.
           MOVE SPACES TO WS-TEL-SAIR.
           PERFORM 01-INFORMA-PARAMETROS
             UNTIL WS-TEL-SAIR = "S"
           goback.
       00-CONTROLE-EXIT.
           EXIT.
           
       01-INFORMA-PARAMETROS SECTION.
           INITIALIZE WS-DADOS-TELA.
           DISPLAY TELA-RELCLI.
           DISPLAY TELA-RELCLI-PRM.
           MOVE SPACES TO WS-MSG-CLI.
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-INFORMA-NOME-ARQUIVO
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-INFORMA-ORDENACAO
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-INFORMA-CLASSIFICACAO
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           MOVE " " TO WS-PARAM-OK
           PERFORM 011-INFORMA-COD-CLIENTE
             UNTIL WS-PARAM-OK EQUAL "OK"
               
           MOVE " " TO WS-PARAM-OK
           PERFORM 011-INFORMA-RAZAO-CLIENTE
             UNTIL WS-PARAM-OK EQUAL "OK"
           
           MOVE " " TO WS-PARAM-OK
           PERFORM 011-INFORMA-COD-VENDEDOR
             UNTIL WS-PARAM-OK EQUAL "OK"

           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-CONFIRMAR
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           IF WS-TEL-VND-CONFIRMAR = "S" THEN
               CALL 'GERAR_REL_CLIENTE' 
                    USING
                    WS-TEL-NMARQUIVO,
                    WS-TEL-PRM-ORD,
                    WS-TEL-PRM-CLA,
                    WS-TEL-PRM-CDCLI,
                    WS-TEL-PRM-RAZAO,
                    WS-TEL-PRM-CDVND
           END-IF.
           
           MOVE "Relatorio gerado no diretorio" TO WS-MSG-CLI.
           DISPLAY TELA-RELCLI-MSG.
           DISPLAY TELA-SAIR.
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-SAIR
             UNTIL WS-PARAM-OK EQUAL "OK".
           
       01-INFORMA-PARAMETROS-EXIT.
           EXIT.
        
       011-INFORMA-NOME-ARQUIVO SECTION.
            ACCEPT  TELA-RELCLI-NMARQ.
            IF WS-TEL-NMARQUIVO EQUAL SPACES THEN
                MOVE "Nome do arquivo deve ser informado" TO WS-MSG-CLI 
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CLI
            END-IF.
            DISPLAY TELA-RELCLI-MSG.
       011-INFORMA-NOME-ARQUIVO-EXIT.
           EXIT.
           
       011-INFORMA-ORDENACAO SECTION.
            ACCEPT  TELA-RELCLI-PRM-ORD.
            IF  WS-TEL-PRM-ORD <> "A"
            AND WS-TEL-PRM-ORD <> "D"
                MOVE "Tipo de ordenacao invalida" TO WS-MSG-CLI         
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CLI
            END-IF.
            DISPLAY TELA-RELCLI-MSG.
       011-INFORMA-ORDENACAO-EXIT.
           EXIT.

       011-INFORMA-CLASSIFICACAO SECTION.
            ACCEPT  TELA-RELCLI-PRM-CLA.
            IF  WS-TEL-PRM-CLA <> "C"
            AND WS-TEL-PRM-CLA <> "R"
                MOVE "Tipo de classificacao invalida" TO WS-MSG-CLI     
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CLI
            END-IF.
            DISPLAY TELA-RELCLI-MSG.
       011-INFORMA-CLASSIFICACAO-EXIT.
           EXIT.

       011-INFORMA-COD-CLIENTE SECTION.
            MOVE "Informe 0000000 para todos" TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.    
            ACCEPT  TELA-RELCLI-PRM-FLT-CODIGO.
            MOVE "OK" TO WS-PARAM-OK.
            MOVE SPACES TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.
       011-INFORMA-COD-CLIENTE-EXIT.
           EXIT.

       011-INFORMA-RAZAO-CLIENTE SECTION.
            MOVE "Informe espaços para todos" TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.
            ACCEPT  TELA-RELCLI-PRM-FLT-NOME.
            MOVE "OK" TO WS-PARAM-OK.
            MOVE SPACES TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.           
       011-INFORMA-RAZAO-CLIENTE-EXIT.
           EXIT.
       
       011-INFORMA-COD-VENDEDOR SECTION.
            MOVE "Informe 000 para todos" TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.    
            ACCEPT  TELA-RELCLI-PRM-FLT-CODVND.                         
            MOVE "OK" TO WS-PARAM-OK.
            MOVE SPACES TO WS-MSG-CLI.
            DISPLAY TELA-RELCLI-MSG.
           
       011-INFORMA-COD-VENDEDOR-EXIT.
           EXIT.
           
       011-CONFIRMAR SECTION.
            ACCEPT  TELA-DADOS-CONFIRMAR-CLI.
            IF  WS-TEL-VND-CONFIRMAR <> "S"
            AND WS-TEL-VND-CONFIRMAR <> "N"
                MOVE "Opcao invalida" TO WS-MSG-CLI
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CLI
            END-IF.
            DISPLAY TELA-RELCLI-MSG.
       011-CONFIRMAR-EXIT.
           EXIT.
           
       011-SAIR SECTION.
            ACCEPT  TELA-DADOS-SAIR.
            IF  WS-TEL-SAIR <> "S"
            AND WS-TEL-SAIR <> "N"
                MOVE "Opcao invalida" TO WS-MSG-CLI
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-CLI
            END-IF.
            DISPLAY TELA-RELCLI-MSG.
       011-SAIR-EXIT.
           EXIT.



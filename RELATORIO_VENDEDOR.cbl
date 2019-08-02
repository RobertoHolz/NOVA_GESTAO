       identification division.
       program-id. "RELATORIO_VENDEDOR".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-vendedor.cpy'.
       
       data division.
       file section.
         copy 'fd-arq-vendedor.cpy'.
       
       working-storage section.
	       01 ws-variaveis.
		      05 wid-arq-vendedor     pic x(22) value SPACES.          
			  05 WS-RST-ACESS-VND     pic 9(02) value ZEROS.            
          
              05 WS-ERRO-ABERTURA     PIC 9(02) VALUE ZEROS.
              05 WS-EXISTE-VENDEDOR   PIC 9(01) VALUE ZEROS.           
              05 ws-acao              pic 9(01) value zeros.
              05 WS-SAIR              PIC 9(01) VALUE ZEROS.
              05 WS-PARAM-OK          PIC X(02) VALUE SPACES.
              05 WS-DADOS-TELA.
                 10 WS-MSG-VND             PIC X(40) VALUE SPACES.
                 10 WS-TEL-NMARQUIVO       PIC X(40) VALUE SPACES.
                 10 WS-TEL-PRM-ORD         PIC X(01) VALUE SPACES.
                 10 WS-TEL-PRM-CLA         PIC X(01) VALUE SPACES.
                 10 WS-TEL-PRM-CDVND       PIC 9(03) VALUES ZEROS.
                 10 WS-TEL-PRM-NMVND       PIC X(40) VALUE SPACES.
                 10 WS-TEL-VND-CONFIRMAR   PIC X(01) VALUE SPACES.
                 10 WS-TEL-SAIR            PIC X(01) VALUE SPACES.
             
       LINKAGE SECTION.
          copy 'lk-gerar-rel-vendedor.cpy'.

       SCREEN SECTION.
       01 TELA-RELVND.
          05 VALUE "--- GERAR RELATORIO DE VENDEDORES ---" BLANK SCREEN 
          LINE 1 COL 35.
          05 TELA-RELVND-MSG.
             10 LINE 22 COL 20 VALUE "Mensagem:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-MSG-VND.           
       

       01 TELA-RELVND-PRM.
         05  TELA-RELVND-NMARQ.
             10 LINE 06 COLUMN 20 VALUE "Arquivo a gerar: ".
             10 COLUMN PLUS 2 PIC X(22) USING WS-TEL-NMARQUIVO.
          05 TELA-RELVND-PRM-ORD.
             10 LINE 08 COL 20 VALUE "Ordenacao (A / D).......:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-PRM-ORD.
          05 TELA-RELVND-PRM-CLA.
             10 LINE 10 COL 20 VALUE "Tipo Classificacao......:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-PRM-CLA.   
             10 LINE 11 COL 20 VALUE "  C - Codigo do Vendedor".
             10 LINE 12 COL 20 VALUE "  N - Nome do Vendedor".
          05 TELA-RELVND-PRM-FLT-CODIGO.
             10 LINE 14 COL 20 VALUE "-- FILTROS --".
             10 LINE 15 COL 20 VALUE "Codigo do Vendedor.. ...:".
             10 COLUMN PLUS 2 PIC 9(03) USING WS-TEL-PRM-CDVND.
          05 TELA-RELVND-PRM-FLT-NOME.
             10 LINE 16 COL 20 VALUE "Nome do Vendedor........:".
             10 COLUMN PLUS 2 PIC X(40) USING WS-TEL-PRM-NMVND.
          05 TELA-DADOS-CONFIRMAR-VND.
             10 LINE 19 COL 20 VALUE "Confirmar(S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-VND-CONFIRMAR.
       01 TELA-SAIR.
          05 TELA-DADOS-SAIR.
             10 LINE 19 COL 20 VALUE "Deseja Sair (S/N).:".
             10 COLUMN PLUS 2 PIC X(01) USING WS-TEL-SAIR.
             
         
       PROCEDURE DIVISION.
       00-CONTROLE section.
           DISPLAY TELA-RELVND.
           DISPLAY TELA-RELVND-PRM.
           MOVE SPACES TO WS-TEL-SAIR.
           PERFORM 01-INFORMA-PARAMETROS
             UNTIL WS-TEL-SAIR = "S"
           goback.
       00-CONTROLE-EXIT.
           EXIT.
           
       01-INFORMA-PARAMETROS SECTION.
           INITIALIZE WS-DADOS-TELA.
           DISPLAY TELA-RELVND.
           DISPLAY TELA-RELVND-PRM.
           MOVE SPACES TO WS-MSG-VND.
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
           PERFORM 011-INFORMA-COD-VENDEDOR
             UNTIL WS-PARAM-OK EQUAL "OK"
               
           MOVE " " TO WS-PARAM-OK
           PERFORM 011-INFORMA-NOME-VENDEDOR
             UNTIL WS-PARAM-OK EQUAL "OK"
           
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-CONFIRMAR
             UNTIL WS-PARAM-OK EQUAL "OK".
           
           IF WS-TEL-VND-CONFIRMAR = "S" THEN
               CALL 'GERAR_REL_VENDEDOR' 
                    USING
                    WS-TEL-NMARQUIVO,
                    WS-TEL-PRM-ORD,
                    WS-TEL-PRM-CLA,
                    WS-TEL-PRM-CDVND,
                    WS-TEL-PRM-NMVND
           END-IF.
           
           MOVE "Relatorio gerado no diretorio" TO WS-MSG-VND.
           DISPLAY TELA-RELVND-MSG.
           DISPLAY TELA-SAIR.
           MOVE " " TO WS-PARAM-OK.
           PERFORM 011-SAIR
             UNTIL WS-PARAM-OK EQUAL "OK".
           
       01-INFORMA-PARAMETROS-EXIT.
           EXIT.
        
       011-INFORMA-NOME-ARQUIVO SECTION.
            ACCEPT  TELA-RELVND-NMARQ.
            IF WS-TEL-NMARQUIVO EQUAL SPACES THEN
                MOVE "Nome do arquivo deve ser informado" TO WS-MSG-VND
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-VND
            END-IF.
            DISPLAY TELA-RELVND-MSG.
       011-INFORMA-NOME-ARQUIVO-EXIT.
           EXIT.
           
       011-INFORMA-ORDENACAO SECTION.
            ACCEPT  TELA-RELVND-PRM-ORD.
            IF  WS-TEL-PRM-ORD <> "A"
            AND WS-TEL-PRM-ORD <> "D"
                MOVE "Tipo de ordenacao invalida" TO WS-MSG-VND
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-VND
            END-IF.
            DISPLAY TELA-RELVND-MSG.
       011-INFORMA-ORDENACAO-EXIT.
           EXIT.

       011-INFORMA-CLASSIFICACAO SECTION.
            ACCEPT  TELA-RELVND-PRM-CLA.
            IF  WS-TEL-PRM-CLA <> "C"
            AND WS-TEL-PRM-CLA <> "N"
                MOVE "Tipo de classificacao invalida" TO WS-MSG-VND
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-VND
            END-IF.
            DISPLAY TELA-RELVND-MSG.
       011-INFORMA-CLASSIFICACAO-EXIT.
           EXIT.

       011-INFORMA-COD-VENDEDOR SECTION.
            MOVE "Informe 000 para todos" TO WS-MSG-VND.
            DISPLAY TELA-RELVND-MSG.    
            ACCEPT  TELA-RELVND-PRM-FLT-CODIGO.
            MOVE "OK" TO WS-PARAM-OK.
            MOVE SPACES TO WS-MSG-VND.
            DISPLAY TELA-RELVND-MSG.
       011-INFORMA-COD-VENDEDOR-EXIT.
           EXIT.

       011-INFORMA-NOME-VENDEDOR SECTION.
            MOVE "Informe espaços para todos" TO WS-MSG-VND.
            DISPLAY TELA-RELVND-MSG.
            ACCEPT  TELA-RELVND-PRM-FLT-NOME.
            MOVE "OK" TO WS-PARAM-OK.
            MOVE SPACES TO WS-MSG-VND.
            DISPLAY TELA-RELVND-MSG.           
       011-INFORMA-NOME-VENDEDOR-EXIT.
           EXIT.
 
       011-CONFIRMAR SECTION.
            ACCEPT  TELA-DADOS-CONFIRMAR-VND.
            IF  WS-TEL-VND-CONFIRMAR <> "S"
            AND WS-TEL-VND-CONFIRMAR <> "N"
                MOVE "Opcao invalida" TO WS-MSG-VND
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-VND
            END-IF.
            DISPLAY TELA-RELVND-MSG.
       011-CONFIRMAR-EXIT.
           EXIT.
           
       011-SAIR SECTION.
            ACCEPT  TELA-DADOS-SAIR.
            IF  WS-TEL-SAIR <> "S"
            AND WS-TEL-SAIR <> "N"
                MOVE "Opcao invalida" TO WS-MSG-VND
            ELSE
                MOVE "OK" TO WS-PARAM-OK
                MOVE SPACES TO WS-MSG-VND
            END-IF.
            DISPLAY TELA-RELVND-MSG.
       011-SAIR-EXIT.
           EXIT.
      
           

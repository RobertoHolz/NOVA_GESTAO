       identification division.
       program-id. "GERAR_REL_CLIENTE".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-cliente.cpy'.
         copy 'select-impresso.cpy'.
         SELECT ARQCLI-SORT ASSIGN TO DISK WID-ARQCLI-SORT.
       
       data division.
       file section.
         copy 'fd-arq-cliente.cpy'.
         copy 'fd-impresso.cpy'.
       
       SD ARQCLI-SORT
          DATA RECORD IS ARQCLI-SORT-REC.
       01 REG-CLI-SORT.
           05 CODCLI-SORT                   PIC 9(07).
           05 CNPJCLI-SORT                  PIC 9(14).
           05 RAZAO-SORT                    PIC X(40).
           05 CODVND-SORT                   PIC 9(03).

       
       working-storage section.
	   01 ws-variaveis.
		      05 wid-arq-cliente       pic x(22) value SPACES.          
              05 WID-ARQCLI-SORT       PIC X(22) VALUE SPACES.          
			  05 WS-RESULTADO-ACESSO   pic 9(02) value ZEROS.        
              05 WS-MSG-VND            PIC X(40) VALUE SPACES.
              05 WS-NM-IMPRESSO        PIC X(22) VALUE SPACES.
              05 WS-RST-ACESS-IMPRESSO PIC 9(02) VALUE ZEROS.
              05 WS-FIM-SORT           PIC 9(01) VALUE ZEROS.
              05 WS-CLASSIFICADO-COD   PIC X(01) VALUE SPACES.
              05 WS-CLASSIFICADO-NOME  PIC X(01) VALUE SPACES.
              05 WS-CLASSIFICADO-VND   PIC X(01) VALUE SPACES.
       
       01 WS-LINHAS-IMPRESSO.
          05 WS-LINHA-CAB-1              PIC X(80) VALUE
             "     RELACAO DE CLIENTES".
          05 WS-LINHA-CAB-2.
             10 FILLER                   PIC X(65) VALUE
             " Codigo CNPJ           Razao Social".
             10 FILLER                   PIC X(15) VALUE
             "Cod.Vendedor".
          05 WS-LINHA-DET-1.
             10 WS-DET-CODCLI            PIC ZZZZZZ9B.
             10 WS-DET-CNPJCLI           PIC ZZZZZZZZZZZZZ9B.
             10 WS-DET-RAZAO             PIC X(40)B.
             10 WS-DET-CODVND            PIC ZZ9.

          
       LINKAGE SECTION.
          copy 'lk-gerar-rel-cliente.cpy'.
       

       PROCEDURE DIVISION USING WL-LINK-GERAR-REL-CLI.
       00-CONTROLE section.
           
           IF WL-PRM-ORD = "A" THEN
              IF  WL-PRM-CLA = "C" THEN
                  SORT ARQCLI-SORT
                    ON ASCENDING KEY CODCLI-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              ELSE
                  SORT ARQCLI-SORT
                    ON ASCENDING KEY RAZAO-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              END-IF
           END-IF.
           
           IF WL-PRM-ORD = "D" THEN
              IF  WL-PRM-CLA = "C" THEN
                  SORT ARQCLI-SORT
                    ON DESCENDING KEY CODCLI-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              ELSE
                  SORT ARQCLI-SORT
                    ON DESCENDING KEY RAZAO-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              END-IF
           END-IF.
           
           goback.
       00-CONTROLE-EXIT.
           EXIT.
           
       01-ENTRADA-SORT SECTION.
           MOVE 0 TO WS-RESULTADO-ACESSO.
           MOVE "ARQ_CLIENTE" TO wid-arq-cliente.
           OPEN INPUT ARQ-CLIENTE.
           IF WS-RESULTADO-ACESSO = 0
               READ ARQ-CLIENTE NEXT RECORD
               PERFORM 011-LER-PROCESSAR-VENDEDOR
                 UNTIL WS-RESULTADO-ACESSO > 0
           END-IF
           CLOSE ARQ-CLIENTE.

       01-ENTRADA-SORT-EXIT.
           EXIT.
           
       011-LER-PROCESSAR-VENDEDOR SECTION.
           
           MOVE "N" TO WS-CLASSIFICADO-COD.
           
           IF WL-PRM-CDCLI EQUAL ZEROS THEN
               MOVE "S" TO WS-CLASSIFICADO-COD                        
           ELSE
               IF WL-PRM-CDCLI = CLI-CODIGO THEN
                  MOVE "S" TO WS-CLASSIFICADO-COD                      
               END-IF
           END-IF.
           
           MOVE "N" TO WS-CLASSIFICADO-NOME.
           IF WL-PRM-RAZAO EQUAL " "
               MOVE "S" TO WS-CLASSIFICADO-NOME                    
           ELSE
               IF WL-PRM-RAZAO = CLI-RAZAOSOCIAL THEN
                   MOVE "S" TO WS-CLASSIFICADO-NOME                
               END-IF
           END-IF.
           
           MOVE "N" TO WS-CLASSIFICADO-VND.
           IF WL-PRM-CDVND EQUAL ZEROS THEN
               MOVE "S" TO WS-CLASSIFICADO-VND                          
           ELSE
               IF WL-PRM-CDVND = CLI-CODVND THEN
                  MOVE "S" TO WS-CLASSIFICADO-VND                       
               END-IF
           END-IF.
           
           
           IF  WS-CLASSIFICADO-COD  = "S"
           AND WS-CLASSIFICADO-NOME = "S" 
           AND WS-CLASSIFICADO-VND  = "S" THEN
              MOVE CLI-CODIGO      TO CODCLI-SORT
              MOVE CLI-CNPJ        TO CNPJCLI-SORT
              MOVE CLI-RAZAOSOCIAL TO RAZAO-SORT
              MOVE CLI-CODVND      TO CODVND-SORT
              RELEASE REG-CLI-SORT 
           END-IF.
           
           READ ARQ-CLIENTE NEXT RECORD.
           
       011-LER-PROCESSAR-VENDEDOR-EXIT.
           EXIT.
           
       02-SAIDA-SORT SECTION.

           MOVE WL-NMARQUIVO TO WS-NM-IMPRESSO.
           OPEN OUTPUT ARQ-IMPRESSO.
           
           MOVE WS-LINHA-CAB-1 TO ARQ-IMPRESSO-LINHA.
           WRITE ARQ-IMPRESSO-LINHA.
           
           MOVE SPACES TO ARQ-IMPRESSO-LINHA.
           WRITE ARQ-IMPRESSO-LINHA.
           
           MOVE WS-LINHA-CAB-2 TO ARQ-IMPRESSO-LINHA.
           WRITE ARQ-IMPRESSO-LINHA.
           
           MOVE 0 TO WS-FIM-SORT.
           PERFORM 021-LINHAS-DETALHE
             UNTIL WS-FIM-SORT = 1.
           
           CLOSE ARQ-IMPRESSO.
           
       02-SAIDA-SORT-EXIT.
           EXIT.
       
       021-LINHAS-DETALHE SECTION.
           RETURN ARQCLI-SORT
             AT END MOVE 1 TO WS-FIM-SORT.
           
           IF  WS-FIM-SORT = 0 THEN
               PERFORM 0211-GRAVA-LINHA-DETALHE
           END-IF.
           
       021-LINHAS-DETALHE-EXIT.
           EXIT.
       
       0211-GRAVA-LINHA-DETALHE SECTION.
           MOVE SPACES TO WS-LINHA-DET-1.
           MOVE CODCLI-SORT TO WS-DET-CODCLI.
           MOVE CNPJCLI-SORT TO WS-DET-CNPJCLI.
           MOVE RAZAO-SORT TO WS-DET-RAZAO.
           MOVE CODVND-SORT TO WS-DET-CODVND.
           MOVE WS-LINHA-DET-1 TO ARQ-IMPRESSO-LINHA.
           WRITE ARQ-IMPRESSO-LINHA.
          
       0211-GRAVA-LINHA-DETALHE-EXIT.
           EXIT.
           


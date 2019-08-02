       identification division.
       program-id. "GERAR_REL_VENDEDOR".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       file-control.
         copy 'select-arq-vendedor.cpy'.
         copy 'select-impresso.cpy'.
         SELECT ARQVND-SORT ASSIGN TO DISK WID-ARQVND-SORT.
       
       data division.
       file section.
         copy 'fd-arq-vendedor.cpy'.
         copy 'fd-impresso.cpy'.
       
       SD ARQVND-SORT
          DATA RECORD IS ARQVND-SORT-REC.
        01 REG-VEN-SORT.
           05 CODVND-SORT                   PIC 9(03).
           05 CPFVND-SORT                   PIC X(11).
           05 NOMVND-SORT                   PIC X(40).
           05 LATVND-SORT                   PIC S9(03)V9(08).
           05 LONVND-SORT                   PIC S9(03)V9(08).
       
       working-storage section.
	   01 ws-variaveis.
		      05 wid-arq-vendedor      pic x(22) value SPACES.
              05 WID-ARQVND-SORT       PIC X(22) VALUE SPACES.
			  05 WS-RST-ACESS-VND      pic 9(02) value ZEROS.   
              05 WS-MSG-VND            PIC X(40) VALUE SPACES.
              05 WS-NM-IMPRESSO        PIC X(22) VALUE SPACES.
              05 WS-RST-ACESS-IMPRESSO PIC 9(02) VALUE ZEROS.
              05 WS-FIM-SORT           PIC 9(01) VALUE ZEROS.
              05 WS-CLASSIFICADO-COD   PIC X(01) VALUE SPACES.
              05 WS-CLASSIFICADO-NOME  PIC X(01) VALUE SPACES.
       
       01 WS-LINHAS-IMPRESSO.
          05 WS-LINHA-CAB-1              PIC X(80) VALUE
             "     RELACAO DE VENDEDORES".
          05 WS-LINHA-CAB-2.
             10 FILLER                   PIC X(59) VALUE
             "Codigo CPF         Nome do Vendedor".
          05 WS-LINHA-DET-1.
             10 WS-DET-CODVND            PIC BBBZZZB.
             10 WS-DET-CPFVND            PIC Z(11)B.
             10 WS-DET-NOMVND            PIC X(40)B.

          
       LINKAGE SECTION.
          copy 'lk-gerar-rel-vendedor.cpy'.
       

       PROCEDURE DIVISION USING WL-LINK-GERAR-REL-VND.
       00-CONTROLE section.
           
           IF WL-PRM-ORD = "A" THEN
              IF  WL-PRM-CLA = "C" THEN
                  SORT ARQVND-SORT
                    ON ASCENDING KEY CODVND-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              ELSE
                  SORT ARQVND-SORT
                    ON ASCENDING KEY NOMVND-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              END-IF
           END-IF.
           
           IF WL-PRM-ORD = "D" THEN
              IF  WL-PRM-CLA = "C" THEN
                  SORT ARQVND-SORT
                    ON DESCENDING KEY CODVND-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              ELSE
                  SORT ARQVND-SORT
                    ON DESCENDING KEY NOMVND-SORT
                    INPUT PROCEDURE IS 01-ENTRADA-SORT
                   OUTPUT PROCEDURE IS 02-SAIDA-SORT
              END-IF
           END-IF.
           
           goback.
       00-CONTROLE-EXIT.
           EXIT.
           
       01-ENTRADA-SORT SECTION.
           MOVE 0 TO WS-RST-ACESS-VND.
           MOVE "ARQ_VENDEDOR" TO wid-arq-vendedor.
           OPEN INPUT ARQ-VENDEDOR.
           IF WS-RST-ACESS-VND = 0
               READ ARQ-VENDEDOR NEXT RECORD
               PERFORM 011-LER-PROCESSAR-VENDEDOR
                 UNTIL WS-RST-ACESS-VND > 0
           END-IF
           CLOSE ARQ-VENDEDOR.

       01-ENTRADA-SORT-EXIT.
           EXIT.
           
       011-LER-PROCESSAR-VENDEDOR SECTION.
           
           MOVE "N" TO WS-CLASSIFICADO-COD.
           
           IF WL-PRM-CDVND EQUAL ZEROS THEN
               MOVE "S" TO WS-CLASSIFICADO-COD                        
           ELSE
               IF WL-PRM-CDVND = VND-CODIGO THEN
                  MOVE "S" TO WS-CLASSIFICADO-COD                      
               END-IF
           END-IF.
           
           MOVE "N" TO WS-CLASSIFICADO-NOME.
           IF WL-PRM-NMVND EQUAL " "
               MOVE "S" TO WS-CLASSIFICADO-NOME                    
           ELSE
               IF WL-PRM-NMVND = VND-NOME THEN
                   MOVE "S" TO WS-CLASSIFICADO-NOME                
               END-IF
           END-IF.
           
           
           IF  WS-CLASSIFICADO-COD  = "S"
           AND WS-CLASSIFICADO-NOME = "S" THEN
              MOVE VND-REGISTRO TO REG-VEN-SORT
              RELEASE REG-VEN-SORT 
           END-IF.
           
           READ ARQ-VENDEDOR NEXT RECORD.
           
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
           RETURN ARQVND-SORT
             AT END MOVE 1 TO WS-FIM-SORT.
           
           IF  WS-FIM-SORT = 0 THEN
               PERFORM 0211-GRAVA-LINHA-DETALHE
           END-IF.
           
       021-LINHAS-DETALHE-EXIT.
           EXIT.
       
       0211-GRAVA-LINHA-DETALHE SECTION.
           MOVE SPACES TO WS-LINHA-DET-1.
           MOVE CODVND-SORT TO WS-DET-CODVND.
           MOVE CPFVND-SORT TO WS-DET-CPFVND.
           MOVE NOMVND-SORT TO WS-DET-NOMVND.
           MOVE WS-LINHA-DET-1 TO ARQ-IMPRESSO-LINHA.
           WRITE ARQ-IMPRESSO-LINHA.
          
       0211-GRAVA-LINHA-DETALHE-EXIT.
           EXIT.
           

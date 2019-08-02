       identification division.
       program-id. "MENU".
       author.     Roberto Holz.
       environment division.
       configuration section.
       input-output section.
       data division.
       working-storage section.
	       01 WS-VARIAVEIS.
		      05 WS-OPCAO-MENU PIC 9(02).
              05 WS-MSG-MENU   PIC X(40).
              
       SCREEN SECTION.
       01 TELA-MENU-PRINCIPAL.
          05 VALUE "--- GERENCIA DE CLIENTES E VENDEDORES ---"
          BLANK SCREEN LINE 2 COL 35.
          05 VALUE "Cadastros"                     LINE 4 COL 35.
          05 VALUE "  01. Cadastro de Clientes"    LINE 5 COL 35.
          05 VALUE "  02. Cadastro de Vendedores"  LINE 6 COL 35.
          05 VALUE "Relatorios"                    LINE 7 COL 35.
          05 VALUE "  03. Relatorio de Clientes"   LINE 8 COL 35.
          05 VALUE "  04. Relatorio de Vendedores" LINE 9 COL 35.
          05 VALUE "Executar"                      LINE 10 COL 35.
          05 VALUE "  05. Executar Distribuicao de Clientes"   
                                                   LINE 11 COL 35.
          05 VALUE "Finalizar"                     LINE 13 COL 35.
          05 VALUE "  99. Finalizar"               LINE 14 COL 35.
          05 VALUE "OPCAO.: "                      LINE 16 COL 35.
          05 TT-OPCAO-MENU            LINE 16 COL 43
                   PIC 9(2)           TO WS-OPCAO-MENU.
          05 VALUE "Mensagem:"                     LINE 20 COL 35.
          05 TT-MSG-MENU                           LINE 20 COL 45
                   PIC X(40)          FROM WS-MSG-MENU.
          
      
       procedure division.
       00-controle section.
	       perform 01-INICIALIZAR.
	       perform 02-PROCESSAR until WS-OPCAO-MENU = 99.
	       perform 03-FINALIZAR.
           STOP RUN.
       00-controle-exit. 
           exit.
       
       01-INICIALIZAR SECTION.
           INITIALIZE WS-VARIAVEIS.
           MOVE "Seja Bem-vindo" TO WS-MSG-MENU.
       01-INICIALIZAR-EXIT. 
           EXIT.
           
       02-PROCESSAR SECTION.
           DISPLAY TELA-MENU-PRINCIPAL.
           ACCEPT TT-OPCAO-MENU.
           EVALUATE WS-OPCAO-MENU
           WHEN '01'
             CALL 'CADASTRO_CLIENTE'
           WHEN '02' 
             CALL 'CADASTRO_VENDEDOR'                                   
           WHEN '03' 
             CALL 'RELATORIO_CLIENTE'                                   
           WHEN '04' 
             CALL 'RELATORIO_VENDEDOR'                                  
           WHEN '05' 
             CALL 'CALC_DISTRIBUICAO'                                   
           WHEN '99' 
             MOVE 'Saindo'              TO WS-MSG-MENU              
           WHEN OTHER                                                   
             MOVE 'Opcao invalida' TO WS-MSG-MENU
           END-EVALUATE.		               
           
       02-PROCESSAR-EXIT. 
           EXIT.
           
       03-FINALIZAR SECTION.
       03-FINALIZAR-EXIT. 
           EXIT.


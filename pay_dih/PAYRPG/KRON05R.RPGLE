000100951207     H DATEDIT(*DMY)
000200110531     H bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300130523     H/define profoundui
000400110531      //*****  PROGRAMMER - S. BIRKETT
000500960415     FKRON05D   CF   E             WORKSTN
000600960416     F                                     SFILE(SFDSP:REC)
000700130523     F/IF DEFINED(WEB)
000800130523     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900130523     F/ENDIF
001000960415     FKRONCF    IF   E           K DISK
001100110531      //
001200110531     Drec              S              4  0
001300951207     D                SDS
001400951207     D  @PRM             *PARMS
001500110531
001600110531     D*--------------------------------------------------
001700110531     D* Program name: Main
001800110531     D* Purpose:
001900110531     D* Returns:
002000110531     D* Parameter:      inClassName
002100110531     D* Parameter:      inClassID
002200110531     D*--------------------------------------------------
002300110531     D Main            PR                  EXTPGM('KRON05R')
002400110531     D  inClassName                  15A
002500110627     D  inClassID                     3P 0 options(*nopass)
002600110531
002700110531     D Main            PI
002800110531     D  inClassName                  15A
002900110627     D  inClassID                     3P 0 options(*nopass)
003000110531
003100110531      //
003200951207     C                   MOVE      '0'           *IN
003300110531      //
003400110531        //
003500110531      /FREE
003600110531         rec = 0;
003700110531         IF @PRM = *ZEROS;
003800110531           *IN99 = *ON;
003900110531         ENDIF;
004000110531         //
004100110531
004200110531         *in12 = *on;
004300110531         WRITE SFCTL;
004400110531         *in12 = *off;
004500110531          SETLL *LOVAL KRONCF;
004600110531          READ KCPREC;
004700110531         *IN13 = %EOF;
004800110531         DOW not %eof;
004900110531           rec += 1;
005000110531
005100110531         WRITE SFDSP;
005200110531         *IN20 = %EOF;
005300110531         READ KCPREC;
005400110531         *IN13 = %EOF;
005500110531         ENDDO;
005600110531           if rec = 0;
005700110531             *in25 = *on;
005800110531           ENDIF;
005900110531
006000110531       EXFMT SFCTL;
006100110531       //
006200110531       IF not *IN25;
006300110531         READC SFDSP;
006400110531         *IN16 = %EOF;
006500110531         IF *IN16 = '0'
006600110531         AND OPT = 'X';
006700110531           inclassname = Kclass;
006800110627            IF @PRM >= %parmnum(inClassID);
006900110627              inClassID = kclassid;
007000110627            endif;
007100110531         ENDIF;
007200110531       ENDIF;
007300110627
007400110531       *INLR = *ON;
007500110531      /END-FREE
007600110531

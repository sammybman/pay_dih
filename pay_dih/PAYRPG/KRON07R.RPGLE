000100951207     H DATEDIT(*DMY)
000200110531     H bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300130523     H/define profoundui
000400110531      //*****  PROGRAMMER - S. BIRKETT
000500960416     FKRON07D   CF   E             WORKSTN
000600960416     F                                     SFILE(SFDSP:REC)
000700130523     F/IF DEFINED(WEB)
000800130523     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000900130523     F/ENDIF
001000960416     FKRONTF    IF   E           K DISK
001100110531      //
001200110531      /copy qcpysrc,genPR
001300110531     Drec              S              4  0
001400951207     D                SDS
001500951207     D  @PRM             *PARMS
001600110531
001700110531     D*--------------------------------------------------
001800110531     D* Procedure name: aainit
001900110531     D* Purpose:
002000110531     D* Returns:
002100110531     D*--------------------------------------------------
002200110531     D aainit          PR
002300110531
002400110531     D*--------------------------------------------------
002500110531     D* Program name: Main
002600110531     D* Purpose:
002700110531     D* Returns:
002800110531     D* Parameter:      Termname
002900110531     D* Parameter:      termid
003000110531     D*--------------------------------------------------
003100110531     D Main            PR                  EXTPGM('KRON07R')
003200110531     D  Termname                     15A
003300110627     D  termid                       10A   options(*nopass)
003400110531
003500110531     D Main            PI
003600110531     D  Termname                     15A
003700110627     D  termid                       10A   options(*nopass)
003800110531
003900110531
004000110531      //
004100951207     C                   MOVE      '0'           *IN
004200110531      //
004300110531      //
004400110531
004500110531      /FREE
004600110531        IF @PRM = *ZEROS;
004700110531          *IN99 = *ON;
004800110531        ENDIF;
004900110531         //
005000110531          *in12 = *on;
005100110531          rec = 0;
005200110531         WRITE SFCTL;
005300110531          *in12 = *off;
005400110531
005500110531         SETLL *LOVAL KRONTF;
005600110531         READ KTGREC;
005700110531        *IN13 = %EOF;
005800110531        DOW *IN13 = '0';
005900110531      /END-FREE
006000951207     C                   ADD       1             REC               4 0
006100110531      /FREE
006200110531         WRITE SFDSP;
006300110531         *IN20 = %EOF;
006400110531         READ KTGREC;
006500110531         *IN13 = %EOF;
006600110531       ENDDO;
006700110531         *in25 = rec = 0;
006800110531       EXFMT SFCTL;
006900110531       //
007000110531       IF *IN25 = *OFF;
007100110531         READC SFDSP;
007200110531         *IN16 = %EOF;
007300110531         IF *IN16 = '0'
007400110531         AND OPT = 'X';
007500110531               termname  = ktermg;
007600110627            if @prm >= %parmnum(termid);
007700110627              termid = ktermid;
007800110627            endif;
007900110531         ENDIF;
008000110531       ENDIF;
008100110531       //
008200110531       *INLR = *ON;
008300110531      /END-FREE
008400110531
008500110531     P*--------------------------------------------------
008600110531     P* Procedure name: aainit
008700110531     P* Purpose:
008800110531     P* Returns:
008900110531     P*--------------------------------------------------
009000110531     P aainit          B
009100110531     D aainit          PI
009200110531
009300110531
009400110531      /FREE
009500110531
009600110531            CONAME = get_coname;
009700110531
009800110531
009900110531      /END-FREE
010000110531     P aainit          E
010100110531
010200110531

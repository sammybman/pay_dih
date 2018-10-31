000100110920     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*no)
000200110920     H/define profoundui
000300000313     F**********************************************************
000400010607     F*                   DISERV  SYSTEM                       *
000500111027     F*                    Payroll                             *
000600980616     F*                                                        *
000700980616     F*                         BY                             *
000800980616     F*                                                        *
000900030310     F*             Diserv Systems                             *
001000000313     F*
001100000313     F*                                                        *
001200980616     F*                                                        *
001300111027     F*       RPG PROGRAM NAME..: PAYR010CR                    *
001400980616     F*                                                        *
001500010607     F*       PROGRAMMER NAME: Samuel Britton                  *
001600010607     F*                        Christopher Williams            *
001700110920     F*       DESIGN DATE....: 2011/09/21                      *
001800110920     F*       MODULES........:                                 *
001900980917     F*       INDICATORS.....:                                 *
002000980917     F*                                                        *
002100980616     F*                                                        *
002200111027     F*  THIS PROGRAM IS DESIGNED  to prepare Bank Info. for   *
002300111027     F*                            electronic transfer         *
002400980616     F*                                                        *
002500980616     F**********************************************************
002600111027     Fpayr010cd cf   e             workstn
002700110920     F/IF DEFINED(WEB)
002800110920     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002900110920     F/ENDIF
003000010607     **
003100110920      /copy QCPYSRC,genPR
003200020226     **
003300110920     D arr             s             40    DIM(2) CTDATA PERRCD(1)
003400111013     D prno            s              6  0
003500111013     D date#           s              8  0
003600111013     D type            s              2  0
003700110920     D errmsg          s             75
003800111013
003900111027     D msg1            c                   'Please select an option now'
004000111027     D msg2            c                   'Invalid option selected'
004100111013
004200111013
004300111013     D Line_command    PR                  extpgm('QCMDEXC')
004400111013     D  command                     220    const
004500111013     D  length                       15  5 const
004600110920
004700111027     D RBC_loc_ranges  pr                  extpgm('PAYR010R01')
004800111013
004900111027     D RBC_loc_maint   pr                  extpgm('PAYR010R02')
005000111013
005100111027     D RBC_loc_Enq     pr                  extpgm('PAYR010R03')
005200111013
005300110920
005400980319     D                sds
005500980917     D  @pgm             *proc
005600110920     D  @user                358    367
005700980603     **
005800020226
005900020226     *****************************************************************
006000111013     ** checking authority to access reports
006100020226     *****************************************************************
006200040207      /free
006300110921
006400110920
006500110920         exsr  aainit;
006600111013
006700111013
006800110920         //*******************
006900110920         //** Program Start
007000110920         //*******************
007100111027          dow 0 = 0;
007200111027            exfmt  fmt1;
007300110921
007400110920            // end of program
007500110920            if  *in03;
007600110920               *inlr = *on;
007700110920               return;
007800110920            endif;
007900110921
008000111027            select;
008100111027              when opt = 0;
008200111027                errmsg = msg1;
008300111027              when opt = 1;
008400111027                RBC_loc_maint();
008500111027              when opt = 2;
008600111027                RBC_loc_ranges();
008700111027              when opt = 3;
008800111027                RBC_loc_enq();
008900111027              other;
009000111027                errmsg = msg2;
009100111027            endsl;
009200111027
009300111027            iter;
009400111013
009500111027          enddo;
009600111013            *inlr = *on;
009700111013            return;
009800110920
009900110920       //*****************************
010000110920       // Session Init
010100110920       //*****************************
010200110920
010300110920        begsr *inzsr;
010400111027          get_session_info('PAY':errmsg:@pgm);
010500110920          if errmsg <> *blanks;
010600110920            show_msg(errmsg);
010700110920            *inlr = *on;
010800110920            return;
010900110920          endif;
011000110920
011100110920        endsr;
011200110920
011300110920
011400110920        //*****************************************************
011500110920        //** Begin subroutine to print header
011600110920        //*****************************************************
011700110920        begsr  AAINIT;
011800111013           CONAME = get_coname;
011900111013           @DATE = get_sys_date(date#);
012000111013           get_prd_info('*prd':*blanks:prno:date#);
012100111013           @title = arr(1);
012200110920        endsr;
012300110920
012400110920      /end-free
012500980917     **
012600980917**   Header to be displayed
012700111027      Republic Bank Location Accounts

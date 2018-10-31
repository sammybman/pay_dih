000100041002     H DATEDIT(*YMD)
000200120126     H bnddir('DISDIR':'WHOUSEDIR') dftactgrp(*no) actgrp(*new)
000300120126     h/define PROFOUNDUI
001801151026      //*********************************************************
001802151026      //              D I S E R V   S Y S T E M S.              *
001803151026      //                  PAYROLL  SYSTEM                       *
001804151026      //                                                        *
001805151026      //                         BY                             *
001806151026      //                                                        *
001807151026      //             Diserv systems Team                        *
001808151026      //             P.O BOX XXXXX, GEORGETOWN,GUYANA           *
001809151026      //
001810151026      //                                                        *
001811151026      //                                                        *
001812151026      //       RPG PROGRAM NAME..: PAYRPT12R                    *
001813151026      //                                                        *
001814151026      //       PROGRAMMER NAME: Samuel Britton                  *
001815151026      //       DESIGN DATE....: 2015/10/12                      *
001816151026      //       MODULES........: *NONE                           *
001817151026      //       INDICATORS.....:                                 *
001818151026      //                                                        *
001819151026      //                                                        *
001820151026      //      Description: Performance Incentive Info.          *
001821151026      //                                                        *
001822151026      //*********************************************************
001900041002     F
002000151026     Fpayrpt12d cf   e             workstn usropn
002100120126     F/IF DEFINED(WEB)
002200120126     F                                     HANDLER('PROFOUNDUI(HANDLER)')
002300120126     F/ENDIF
002400030410     F
002500020131     ** General/Payroll PROTOTYPES
002600120126      /copy QCPYSRC,genPR
002700120126      /copy QCPYSRC,autPR
002800060211
002900151026     D  run_report     pr                  extpgm('PAYPICALC')
003000060211     D   fprno                        6  0 const
003100060211     D   tprno                        6  0 const
003300060211
003400020131     **
003500960831     D                SDS
003600960831     D  @PGM             *PROC
003700030410     D  @parm            *parms
003800120126     D  @USER                358    367
003900960904     D  @STIME               282    287  0
004000020131     **
004100030410     D ARR             S             40    DIM(1) CTDATA PERRCD(1)
004200060509     D no_prd          S              6  0
004400120126     D aptr            s               *   inz(*null)
004500030410     **
004600050610     d comp            c                   'Report Completed as Requested'
004700060211     D msg1            c                   'Start Prd. Must be Before End Prd.'
004800060509     D msg2            c                   'More than 12 periods specified'
004900041002     **
005000151026     D main_pgm        PR                  extpgm('PAYRPT12R')
005100050610     **
005200050610     D main_pgm        PI
005300050610
005400041002     d
005500041002     **
005600041002      /free
005700120126
005800120126          // checking authority
005900120126         monitor;
006000151026          authority_check('PAY':@pgm);
006100120126         on-error;
006200120126          show_msg(get_err_msg('DIH0054'));
006300120126          *inlr = *on;
006400120126          return;
006500120126         endmon;
006600120126
006700120126
006900041002
007000050610           exsr aainit;
007100041002
007200151026           open payrpt12d;
007300060211
007400050610         dow   0 = 0;
007500050610
007600060211           exfmt fmt1;
007700041002
007800041002
007900041002           // F3 = End Program
008000041002                     if  *in03;
008100041002                       *inlr = *on;
008200041002                       return;
008300041002                     endif;
008400041002
008500041002
008600060509            // Ensure valid start period
008700060211                if tprno = *zeros;
008800060211                   tprno = fprno;
008900060509                endif;
009000060509
009100060509            // Ensure start period is less than end period
009200060509                 if  fprno > tprno;
009300060509                   errmsg = msg1;
009400060509                   iter;
009500060509                 endif;
009600041002
009700060509            // Ensure maximum periods specified is 12
009800060509                 no_prd = diff_prd(tprno:fprno);
009900060509                 if  no_prd > 12;
010000060509                   errmsg = msg2;
010100060509                   iter;
010200060509                 endif;
010300060211
010400151026                 run_report(get_new_prd(fprno:1:'S'):tprno);
010500060211
010600060211                 errmsg = comp;
010700060211
010800041002
010900041108
011000060211         enddo;
011100060211
011200151026           // close payrpt12d;
011300120126
011400120126         //*****************************
011500120126         // Session Init
011600120126         //*****************************
011700120126
011800120126          begsr *inzsr;
011900120126
012000151026              get_session_info('PAY':errmsg:@pgm);
012200120126
012300120126              if errmsg <> *blanks;
012400120126                show_msg(errmsg);
012500120126                *inlr = *on;
012600120126                return;
012700120126              endif;
012800120126
012900120126
013000120126          endsr;
013100120126
013200050610
013300041002         //***************************************************
013400041002         //** FIRST INITILISATION ROUTINE FOR ALL PROGRAMS  **
013500041002         //***************************************************
013600041002                 begsr aainit;
013700041002                   coname = get_coname;
013800041002                   @title = arr(1);
013900041002                   @date = get_sys_date;
014000041002                 endsr;
014200030410**
014300151026       Performance Incentive Report

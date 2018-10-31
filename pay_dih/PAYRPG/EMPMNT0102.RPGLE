000100951017     H DATEDIT(*YMD)
000200111013     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'CCREDITDIR')
000300111013     H/define profoundui
000400050511     F**Created by - allison fung
000500050511     F**Date       - 11.05.2005
000600050527     Fempsmf    if   e           k disk
000700050530     Fempmnt01d2CF   e             Workstn
000800050513     F                                     sfile(sfile:rrn)
000900111013     F/IF DEFINED(WEB)
001000111013     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111013     F/ENDIF
001200020206     **
001300111013      /copy QCPYSRC,genpr
001400111013      /copy QCPYSRC,glpr
001500111013      /copy QCPYSRC,payPR
001600111013      /copy QCPYSRC,autpr
001700111013      /copy QCPYSRC,emppr
001800111013      /copy QCPYSRC,salpr
001900021004     **
002000980213     d  arr            s             40    dim(2) ctdata perrcd(1)
002100050511     D*run_list        s              1    dim(2)
002200020215     D cnt             s              2  0
002300050513     d rrn             s              4  0
002400020215     D error           s               n
002500050513     D found_ind       s               n
002600050527     D counter         s              5  0
002700020215     **
002800050529     D msg1            c                   'Must enter Amount or Percentage'
002900050512     D msg2            c                   'Error found during update'
003000020206     **
003100941215     D                SDS
003200941215     D  @PGM             *PROC
003300111013     D  @USER                358    367
003400990322     **
003500111013     D main_pgm        PR                  extpgm('EMPMNT0102')
003600050513     D                                2  0 const
003700050519     D                                1    const options(*nopass)
003800050513     D main_pgm        PI
003900050517     D emseltype#                     2  0 const
004000050519     D emsubtyp#                      1    const options(*nopass)
004100050513     **
004200050527     D*-----------------------------------------
004300050527     D* Prototype for procedure: ClearSubfile
004400050527     D*-----------------------------------------
004500050527     D ClearSubfile    PR
004600050527     D
004700050527     D*-----------------------------------------
004800050527     D* Prototype for procedure: LoadSubfile
004900050527     D*-----------------------------------------
005000050527     D LoadSubfile     PR
005100050527     D
005200050527     D*-----------------------------------------
005300050527     D* Prototype for procedure: DisplaySubfile
005400050527     D*-----------------------------------------
005500050527     D DisplaySubfile  PR
005600050527     D
005700050512
005800050513     D*--------------------------------------
005900050530     D* Prototype for procedure: setup_ded
006000050513     D*--------------------------------------
006100050530     D setup_ded       PR
006200050513
006300050513     D*--------------------------------------
006400050530     D* Prototype for procedure: confirm_setup
006500050513     D*--------------------------------------
006600050530     D confirm_setup   PR
006700050513
006800050511     **
006900040207      /free
007000990322
007100050530                  dow 0 = 0;
007200050530                  exsr aainit;
007300050517
007400050519            // Display Sub-Type, if any, for selection
007500050519                     if emsubtyp# = 'Y';
007600050530                       lst_lstype(lrul3styp#);
007700050530                         if lrul3styp# = *zeros;
007800050527                         iter;
007900050527                         endif;
008000050519                     endif;
008100050519
008200050530                    setup_ded();
008300050531                    *inlr = *on;
008400050531                     return;
008500050517
008600050527            enddo;
008700050512         //***************************************************
008800050512         //** First initilisation routine for all programs  **
008900050512         //***************************************************
009000050512                 begsr aainit;
009100050512                  CONAME = get_coname;
009200050512                   @DATE = get_sys_date;
009300050512                     @title = arr(1);
009400050512                 endsr;
009500050512
009600050512      /end-free
009700050527
009800050517     P*--------------------------------------
009900050530     P* Procedure name: setup_ded
010000050517     P* Purpose:        load classes to enforce limits
010100050517     P* Returns:        error indicator
010200050517     P* Parameter:      errmsg => error message text
010300050517     P*--------------------------------------
010400050530     P setup_ded       B
010500050530     D setup_ded       PI
010600050517     D*errmsg                        75A
010700050517
010800050517     C* Your calculation code goes here
010900050517      /free
011000050517
011100050527            ClearSubfile();
011200050527
011300050527            LoadSubfile();
011400050527
011500050527            DisplaySubfile();
011600050527
011700050527      /end-free
011800050530     P setup_ded       E
011900050527
012000050527     P*--------------------------------------
012100050527     P* Procedure name: ClearSubfile
012200050527     P* Purpose:  Clear subfile records
012300050527     P* Returns:        *none
012400050527     P* Parameter:      *none
012500050527     P*--------------------------------------
012600050527     P ClearSubfile    B
012700050527     D ClearSubfile    PI
012800050527      /free
012900050527
013000050527                *in33 = *on;
013100050527                 write footer;
013200050531                 write sfctl;
013300050527                 rrn = 0;
013400050527                 *in33 = *off;
013500050517
013600050527                 counter = *zeros;
013700050527
013800050527      /end-free
013900050527     P ClearSubfile    E
014000050527
014100050527     P*--------------------------------------
014200050527     P* Procedure name: LoadSubfile
014300050527     P* Purpose:  Load subfile records
014400050527     P* Returns:        *none
014500050527     P* Parameter:      *none
014600050527     P*--------------------------------------
014700050527     P LoadSubfile     B
014800050527     D LoadSubfile     PI
014900050527      /free
015000050527
015100050530              lrul3typ# = emseltype#;
015200050530              get_ltype_info('RD':emseltype#:ematf_struc);
015300050530              get_lstype_info('RD':lrul3typ#:lrul3styp#:eastf_struc);
015400050530
015500050527               setll *loval esmfmt;
015600050527               read esmfmt;
015700050527               dow not(%eof);
015800050527
015900050517           // Get transaction records
016000050530              if get_paded_info('RD':lrul3typ#:lrul3styp#:
016100050530                                    esmclass:paded_struc);
016200050530                 clear paded_struc;
016300050527              endif;
016400050517
016500050517          // write to subfile
016600050517                     rrn += 1;
016700050517                      write sfile;
016800050517
016900050527                   read esmfmt;
017000050527               enddo;
017100050517
017200050527                  counter = rrn;
017300050517                  *in55 = rrn > 0;
017400050517
017500050527      /end-free
017600050527     P LoadSubfile     E
017700050527
017800050527     P*--------------------------------------
017900050527     P* Procedure name: DisplaySubfile
018000050527     P* Purpose:  Display subfile records
018100050527     P* Returns:        *none
018200050527     P* Parameter:      *none
018300050527     P*--------------------------------------
018400050527     P DisplaySubfile  B
018500050527     D DisplaySubfile  PI
018600050527      /free
018700050527            dow 0=0;
018800050527
018900050527            *in75 = rrn > 0;
018901141203
018902141203      /if not defined(web)
019000050527            write  footer;
019001141203      /endif
019100050527            exfmt  sfctl;
019200050527
019300050531           // F2= End process
019400050530                   if  *in02;
019500050531                     *inlr = *on;
019600050531                      return;
019700050527                   endif;
019800050527
019900050530           // F15= Update File
020000050527                   if  *in15;
020100050530                      confirm_setup();
020200050531                      *inlr = *on;
020300050530                      return;
020400050527                   endif;
020500050527            enddo;
020600050527
020700050527      /end-free
020800050527     P DisplaySubfile  E
020900050527
021000050517     P*--------------------------------------
021100050530     P* Procedure name: confirm_setup
021200050530     P* Purpose:        confirm changes to criteria setup
021300050517     P* Returns:        error indicator
021400050517     P* Parameter:      errmsg => error message text
021500050517     P*--------------------------------------
021600050530     P confirm_setup   B
021700050530     D confirm_setup   PI
021800050517     D*errmsg                        75A
021900050527     D cnt             s              2  0
022000050527     d y               s              2  0
022100050530     d paded_struc#    ds                  likeds(paded_struc)
022200050517
022300050517     C* Your calculation code goes here
022400050517      /free
022500050517
022600050527              cnt = rrn;
022700050527
022800050527              *in99 = *on;
022900050527
023000050527              for  y = 1 to cnt;
023100050527
023200050527                chain y sfile;
023300050527
023400050530                 if get_paded_info('RD':lrul3typ#:lrul3styp#:
023500050531                                    esmclass:paded_struc#);
023600050531                    if lrul3ded <> *zeros;
023700050527
023800050527                   // if a new record
023900050530                       lrul3sts = 'A';
024000050530                       lrul3type = lrul3typ#;
024100050530                       lrul3stype = lrul3styp#;
024200050530                       lrul3class = esmclass;
024300050530                       get_paded_info('WR':lrul3typ#:lrul3styp#:
024400050530                                    esmclass:paded_struc);
024500050527                  endif;
024600050527
024700050527                else;
024800050527
024900050527               select;
025000050531                  when lrul3ded = *zeros;
025100050531                    get_paded_info('DL':lrul3typ#:lrul3styp#:esmclass);
025200050531
025300050530
025400050527                  other;
025500050531                       lrul3sts = 'A';
025600050531                       lrul3type = lrul3typ#;
025700050531                       lrul3stype = lrul3styp#;
025800050530                    lrul3class = esmclass;
025900050530                    get_paded_info('UP':lrul3typ#:lrul3styp#:
026000050530                                   esmclass:paded_struc);
026100050527               endsl;
026200050527
026300050527                endif;
026400050527              endfor;
026500050517
026600050517      /end-free
026700050517
026800050530     P confirm_setup   E
026900980213**
027000050530        SETUP PAYROLL DEDUCTIONS
027100980213XXXXXXXXXX0123456789XXXXXXXXXX0123456789

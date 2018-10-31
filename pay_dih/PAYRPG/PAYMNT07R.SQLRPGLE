000100940805     H DFTNAME(RTIME)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400090415      //****** PROGRAMMER - S.BRITTON         ************
000500090415      //****** PROGRAMMER - C.Williams        ************
000600111002     Fpayrdefl01uF a e           k disk    usropn
000700111002     Fpayrtypl01IF   e           k disk    usropn
000800010427     Fpaymnt07d CF   e             WORKSTN
000900111002     F                                     sfile(sflrec:rrn)
001000111002     F/IF DEFINED(WEB)
001100111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001200111002     F/ENDIF
001300090415      // Payroll PROTOTYPES
001400111002      /copy QCPYSRC,genPR
001500111002      /copy QCPYSRC,PAYPR
001600111002      /copy QCPYSRC,autPR
001700111002      /copy QCPYSRC,empPR
001800111002
001900010426     D rrn             S              4  0
002000010426     D arr             S             40    DIM(1) CTDATA PERRCD(1)
002100050525     D countr          S              5  0
002200131205     D revertdate      S               d   datfmt(*iso)
002300131204
002400131203     D YES             c                   'YES'
002500090415     D TEXT            c                   'Charge Allowance'
002600131203     D TEXT2           c                   'is no longer selected'
002700131203     D TEXT3           c                   'is now set to'
002800010724     D msg1            c                   'Invalid Run Type'
002900010724     D msg2            c                   'Run Type cannot be edited'
003000131204
003100980209     D                SDS
003200980209     D  @PGM             *PROC
003300131204     D  @user                358    367
003400131204
003500010426     C     trn_key       klist
003600010426     C                   kfld                    run_type
003700010426     C                   kfld                    ptyptype
003800090415      //  Confirm key
003900010505     C     cfm_key       klist
004000010505     C                   kfld                    run_type
004100010505     C                   kfld                    emol_typ#
004200090415      //
004300011106     C                   move      '0'           *IN
004400090415      //******************************
004500090415      // get startup info
004600090415      //******************************
004700040207      /free
004800111002         // authority check
004900111002         monitor;
005000111002           authority_check('PAY':@pgm);
005100111002         on-error;
005200111002           show_msg(get_err_msg('DIH0054'));
005300111002           *inlr = *on;
005400111002           return;
005500111002         endmon;
005600090415
005700090415       exsr aainit;
005800090415       DOw 0 = 0;
005900090415         EXFMT fmt1;
006000090415         // clear errmsg
006100090415         clear errmsg;
006200090415         *in25 = '0';
006300090415
006400090415         if *in03;
006500090415           *inlr = '1';
006600090415           return;
006700090415         endif;
006800090415
006900090415         CHAIN run_type prtyfmt;
007000090415         *IN77 = NOT %FOUND;
007100090415         // Invalid Run Type
007200090415         if *in77;
007300090415           errmsg = msg1;
007400090415           iter;
007500090415         endif;
007600090415         // Run type cannot be edited
007700090415         if prtyprno = 0;
007800090415           errmsg = msg2;
007900090415           iter;
008000090415         endif;
008100090415         // Get name for screen
008200090415         run_name = prtydesc;
008300010427
008400090415         // clear subfile and reset all values
008500090415         *in33 = '1';
008600090415         write sflctl;
008700090415         rrn = 0;
008800090415         *in33 = '0';
008900010427
009000050525         *in60 = *off;
009100050525          countr = *zeros;
009200050525           dow not get_payty_info('RD':-1:payty_struc:countr);
009300050524
009400090415           if ptypaytyp = 'S';
009500090415               desc# = TEXT;
009600090415             else;
009700090415               desc# = *blanks;
009800090415           endif;
009900090415
010000090415         //if ptypaytyp <> 'S';
010100090415           if ptylink = *blanks
010200090415                 or ptylink = run_type;
010300090415             chain trn_key prdffmt;
010400090415             //
010500090415             if %found;
010600090415               dftopt = prdfflag;
010700090415             else;
010800090415               dftopt = *blank;
010900090415             endif;
011000090415             // Check if payroll type can be included for run type
011100090415             *in99 = validate_dft(run_type:ptyptype);
011200090415             //* temp code--20060516
011300090415             //*                 if        ptyptype = 03
011400090415             //*                 eval      *in99 = *off
011500090415             //*                 endif
011600090415             //* temp code--20060516
011700090415             //
011800090415             rrn = rrn + 1;
011900090415             emol_typ# = ptyptype;
012000090415             emol_name = ptydesc;
012100090415             write sflrec;
012200090415           endif;
012300090415         //endif;
012400090415       enddo;
012500131216
012600050525         *in60 = *on;
012700131216
012800131216
012900090415       leave;
013000090415       enddo;
013100010426
013200090415       //******************************
013300090415       // Process transactions
013400090415       //******************************
013500090415       dow 0 = 0;
013600010426
013700090415         write footer;
013800090415         exfmt sflctl;
013900090415         // End Program
014000090415         if *in03;
014100090415           *inlr = '1';
014200090415           return;
014300090415         endif;
014400090415         // go back
014500090415         if *in02;
014600090415           leave;
014700090415         endif;
014800090415         // Confirm transactions
014900090415         if *in15;
015000090415           exsr confirm_rtn;
015100090415           leave;
015200090415         endif;
015300010426
015400090415       enddo;
015500010426
015600111002
015700111002         //*****************************
015800111002         // Session Init
015900111002         //*****************************
016000111002
016100111002          begsr *inzsr;
016200111006            get_session_info('PAY':errmsg:@pgm);
016300111002            if errmsg <> *blanks;
016400111002              show_msg(errmsg);
016500111002              *inlr = *on;
016600111002              return;
016700111002            endif;
016800111002
016900111002            open(e) payrtypl01;
017000111002            open(e) payrdefl01;
017100111002
017200111002          endsr;
017300111002
017400090415       begsr confirm_rtn;
017500090415         readc sflrec;
017600090415         *IN55 = %EOF;
017700140102
017800090415         dow not(*in55);
017900090415           chain cfm_key prdffmt;
018000131129
018100131129           // Handle Changes that are not Permanent
018200131129           if %found and dftopt <> prdfflag
018300131129            or not %found and dftopt <> *blanks;
018400131203
018500131203              // set screen info.
018600131203              if dftopt = *blanks;
018700131203                 ttext = %trim(emol_name) + ' ' + TEXT2;
018800131203                else;
018900131203                 ttext = %trim(emol_name) + ' ' + TEXT3 + ' ' + dftopt;
019000131203              endif;
019100131203
019200131203              dou Permflag = 'YES' or numdays <> *zeros
019300131203                or revertflag = 'Y';
019400131203                 exfmt winrec;
019500131129              enddo;
019600131203
019700131203              select;
019800131203                when permflag = YES;
019900131204                  numdays = *zeros;
020000131204                  revertflag = *blanks;
020100131204                  reset revertdate;
020200131216
020300131216                  exec sql
020400131216                     update payrtchglg set prcsts = 'D'
020500131216                      where  prcsts = 'A' and prcrtype = :run_type and
020600131218                      prcptype = :emol_typ# with NC;
020700131216
020800131204
020900131204                other;
021000131204
021100131204                    if numdays <> *zeros;
021200131204                       revertflag = *blanks;
021300131204                       revertdate = %date() + %days(numdays);
021400131205                       else;
021500131205                       revertdate = %date();
021600131204                    endif;
021700131204
021800131216
021900131218                    exec sql
022000131218                       merge into payrtchglg as log
022100131218                           using  (values ('A', :run_type, :emol_typ#,
022200131218                                          :numdays, :revertflag, :prdfflag,
022300131218                                          :dftopt, current timestamp, :@user,
022400131218                                          :revertdate))
022500131218                              NEWROW (prcsts, prcrtype, prcptype, prcndays,
022600131218                                      prcnrflag, prcpvalue, prcnvalue,
022700131218                                      prcdatecrt, prccrtby, prcdrevert)
022800131218
022900131218                            on log.prcrtype = NEWROW.prcrtype
023000131218                             and log.prcptype = NEWROW.prcptype
023100131218                               and log.prcsts = NEWROW.prcsts
023200131218
023300131218                       when MATCHED then
023400131218                         update  set prcndays =  NEWROW.prcndays,
023500131218                                  prcnrflag = NEWROW.prcnrflag,
023600131218                                  prcpvalue = NEWROW.prcpvalue,
023700131218                                  prcnvalue = NEWROW.prcnvalue,
023800131218                                  prcdatecrt = NEWROW.prcdatecrt,
023900131218                                  prccrtby = NEWROW.prccrtby,
024000131218                                  prcdrevert = NEWROW.prcdrevert
024100131218                       when not MATCHED THEN
024200131218                         insert values(NEWROW.prcsts, NEWROW.prcrtype,
024300131218                                      NEWROW.prcptype, NEWROW.prcnrflag,
024400131218                                      NEWROW.prcndays, NEWROW.prcpvalue,
024500131218                                      NEWROW.prcnvalue, NEWROW.prcdatecrt,
024600131218                                      NEWROW.prccrtby, NEWROW.prcdrevert)
024700131218                               with NC;
024800131216
024900131204
025000131203              endsl;
025100131203
025200131204              // clear fields
025300131204              permflag = *blanks;
025400131129
025500131129           endif;
025600131129
025700131129
025800131129           // Update Table
025900090415           select;
026000010427
026100090415           when %found and dftopt = 'Y' or
026200090415                 %found and dftopt = 'X';
026300090415             prdfflag  = dftopt;
026400090415             update prdffmt;
026500010427
026600090415           when %found and dftopt <> 'Y'
026700090415                 and dftopt <> 'X';
026800090415             delete prdffmt;
026900010427
027000090415           when not %found and dftopt = 'Y' or
027100090415                 not %found and dftopt = 'X';
027200090415             prdfsts  = 'A';
027300090415             prdfptype = run_type;
027400090415             prdftype  = emol_typ#;
027500090415             prdfflag  = dftopt;
027600090415             write prdffmt;
027700090415           endsl;
027800140102
027900090415           readc sflrec;
028000090415           *IN55 = %EOF;
028100090415         enddo;
028200090415       endsr;
028300990413
028400090415       //*************************************************
028500090415       // First initilisation routine for all programs  **
028600090415       //*************************************************
028700090415       BEGSR AAINIT;
028800090415         CONAME = get_coname;
028900090415         @DATE = get_sys_date;
029000090415      /END-FREE
029100010426     C                   time                    @TIME             6 0
029200090415      // select header for program
029300010426     C                   movel(p)  ARR(1)        @TITLE
029400090415      //
029500090415      /FREE
029600090415       ENDSR;
029700090415       //
029800090415      /END-FREE
029900010426**  Program Title
030000010427      Payroll Run Type Defaults

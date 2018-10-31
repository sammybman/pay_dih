000100010904     H DATEDIT(*ymd)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300111002     H/define profoundui
000400111002     F* Payroll book program
000500111002     Fpayrtyp   iF   E           K DISK    usropn
000600111002     Fdeptm     iF   e           K DISK    usropn
000700111002     Fpayhisl07 iF   e           K DISK    usropn
000800010905     Fpayrpt03d cf   e             workstn
000900111002     F/IF DEFINED(WEB)
001000111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001100111002     F/ENDIF
001200010905     FQSYSPRT   O    f  132        PRINTER OFLIND(*INOF)
001300110729      /copy qcpysrc,genpr
001400110729      /copy qcpysrc,autpr
001500010904     Damount           S              5  0
001600010903     Dhold_type        S              1
001700010904     Drun_total        s              5  0
001800010904     Dcnt              s              5  0
001900010903     Drun_date         S              8  0
002000010903     Drun_name         S                   like(prtydesc)
002100010904
002200010904     ** system data structure
002300950530     D                SDS
002400010904     D  @pgm             *PROC
002500111002     D  @user                358    367
002600010903     ** system fields for report
002700010903     D@date            S             10
002800010905     D@rpt_title       S             60
002900010903     Dconame           S             30
003000010905     D arr             S             40    DIM(2) CTDATA PERRCD(1)
003100010904     ** array to store run type totals
003200010904     D run_totals      S              5  0 DIM(15)
003300010904     D run_names       S             25    DIM(15)
003400010905     Ddate_err         c                   'Invalid Date Entered'
003500010905     **
003600980126     C
003700010903     C     his_key       klist
003800010903     C                   kfld                    run_date
003900010903     C                   kfld                    dept
004000980203     C
004100040207      /free
004200111002         // authority check
004300111002         monitor;
004400111002           authority_check('PAY':@pgm);
004500111002         on-error;
004600111002           show_msg(get_err_msg('DIH0054'));
004700111002           *inlr = *on;
004800111002           return;
004900111002         endmon;
005000040207      /end-free
005100010903     C                   exsr      aainit
005200010905     C***************************************************************
005300010905     ***         Prepare Report Criteria                          ***
005400010905     C***************************************************************
005500010905
005600010905     C                   dow       0 = 0
005700010905     C                   exfmt     fmt1
005800010905     C                   eval      @rpt_title = %trim(ARR(1)) +  ' '
005900010905     C                                     + %editc(run_date:'Y')
006000010905     C                   clear                   errmsg
006100010905     C                   eval      *in25 = *off
006200010905     ** F3 = End Pgm
006300010905     C                   if        *in03
006400010905     C                   eval      *inlr = '1'
006500010905     C                   return
006600010905     C                   endif
006700010905
006800010905     ** validating date field
006900010905     C                   test(de)                run_date
007000010905     C                   if        %error
007100010905     C                   eval      errmsg = date_err
007200010905     C                   eval      *in25 = *on
007300010905     C                   iter
007400010905     C                   endif
007500010905
007600010905     C                   leave
007700010905     C                   enddo
007800010905
007900010904     C                   exsr      load_sr
008000010903     C                   EXCEPT    hedr
008100010903     C     *loval        setll     dhrec
008200010903     C                   read      dhrec
008300010903     C
008400010903     C                   except    dept_hdr
008500010903     C                   DOW       not(%eof(deptm))
008600010903     C     his_key       SETLL     phisfmt
008700010903     C     his_key       reade     phisfmt
008800010903
008900010903     C                   eval      hold_type = phistype
009000010903     C                   Dow       not(%eof(payhisl07))
009100010903
009200010903
009300010903     C                   if        hold_type <> phistype
009400010903     c     hold_type     chain     prtyfmt
009500010903     C                   if        %found
009600010903     C                   eval      run_name = prtydesc
009700010904     C                   eval      cnt  = 1
009800010904     C     run_name      lookup    run_names(cnt)                         99
009900010904     C                   if        *in99
010000010904     C                   eval      run_totals(cnt)= run_totals(cnt) + amount
010100010904     C                   endif
010200010903     c                   endif
010300010904
010400010904
010500010903     C                   except    details
010600010903     C                   eval      amount = 0
010700010903     C                   eval      hold_type = phistype
010800010903     C                   endif
010900010903
011000010903     C                   eval      amount = amount + 1
011100010903     C                   if        *inof
011200980205     C                   EXCEPT    hedr
011300980205     c                   endif
011400981202     C
011500010903     C     his_key       reade     phisfmt
011600010903     C                   ENDDO
011700010903
011800010903     c     hold_type     chain     prtyfmt
011900010903     C                   if        %found
012000010903     C                   eval      run_name = prtydesc
012100010904     C                   eval      cnt  = 1
012200010904     C     run_name      lookup    run_names(cnt)                         99
012300010904     C                   if        *in99
012400010904     C                   eval      run_totals(cnt)= run_totals(cnt) + amount
012500010904     C                   endif
012600010903     c                   endif
012700010903     C                   except    details
012800010903     C                   eval      amount = 0
012900010903
013000010903     C                   read      dhrec
013100010903     C                   except    dept_hdr
013200980203     C                   ENDDO
013300980203     C
013400010903     C                   EXCEPT    Gtot
013500010904     C                   exsr      sum_sr
013600010904     C                   Except    Repend
013700980203     C
013800980126     C                   seton                                        lr
013900980126     C                   return
014000111002      /free
014100111002
014200111002         //*****************************
014300111002         // Session Init
014400111002         //*****************************
014500111002
014600111002          begsr *inzsr;
014700111006            get_session_info('PAY':errmsg:@pgm);
014800111002            if errmsg <> *blanks;
014900111002              show_msg(errmsg);
015000111002              *inlr = *on;
015100111002              return;
015200111002            endif;
015300111002
015400111002            open(e) payrtyp;
015500111002            open(e) payhisl07;
015600111002            open(e) deptm;
015700111002
015800111002          endsr;
015900111002
016000111002      /end-free
016100010903     ***************************************************
016200010903     C     sum_sr        begsr
016300010904     C                   eval      cnt = 1
016400010904     C     *loval        SETLL     prtyfmt
016500010904     C                   read      prtyfmt
016600010904     C                   Dow       not(%eof(payrtyp))
016700010904     C     prtydesc      lookup    run_names(cnt)                         99
016800010904     C                   if        *in99
016900010904     C                   eval      run_name = run_names(cnt)
017000010904     C                   eval      run_total = run_totals(cnt)
017100010904     C                   except    totrec
017200010904     C                   eval      *in26 = '0'
017300010904     C                   endif
017400010904     c                   read      prtyfmt
017500010904     C                   Enddo
017600010903     C                   endsr
017700010904     ***************************************************
017800010904     C     load_sr       begsr
017900010904     C     *loval        SETLL     prtyfmt
018000010904     C                   read      prtyfmt
018100010904     C                   Dow       not(%eof(payrtyp))
018200010904     C                   eval      cnt = cnt + 1
018300010904     C                   eval      run_names(cnt) = prtydesc
018400010904     c                   read      prtyfmt
018500010904     C                   Enddo
018600010904     C                   endsr
018700010903     ***************************************************
018800010903     ** First initilisation routine for all programs  **
018900010903     ***************************************************
019000010903     C     AAINIT        BEGSR
019100010903     C                   eval      coname = get_coname
019200010903     C                   eval      @date = get_sys_date
019300010903     C     hold_type     chain     prtyfmt
019400010903     ** select header for program
019500010905     C                   eval      @title = ARR(2)
019600010903     **
019700010903     C                   ENDSR
019800950629     **
019900980204     OQSYSPRT   e            hedr           1 01
020000980204     O         or          of
020100950629     O                       @PGM                10
020200010903     O                       coname              70
020300980204     O          e            hedr           1
020400980204     O         or          of
020500980204     O                       @USER               10
020600010905     O                       @rpt_title          91
020700950629     O                                          120 'RUN Date: '
020800010903     O                       @date             +  1
020900980204     O          e            hedr           1
021000980204     O         or          of
021100980817     O                                           10 '---------'
021200980127     O                                         +  1 '---------'
021300980127     O                                         +  1 '---------'
021400980127     O                                         +  1 '---------'
021500980127     O                                         +  1 '---------'
021600980127     O                                         +  1 '---------'
021700980127     O                                         +  1 '---------'
021800980127     O                                         +  1 '---------'
021900980131     O                                         +  1 '---------'
022000980204     O          e            hedr           1
022100980204     O         or          of
022200980817     O                                           10 '| DEPT#  '
022300010903     O                                           50 'PAYSLIPS '
022400980817     O                                         +  2 'PAYPACKET'
022500980817     O                                         +  1 ' RECEIPT '
022600980204     O          e            hedr           1
022700980204     O         or          of
022800980817     O                                           10 '---------'
022900980131     O                                         +  1 '---------'
023000980131     O                                         +  1 '---------'
023100980131     O                                         +  1 '---------'
023200980131     O                                         +  1 '---------'
023300980131     O                                         +  1 '---------'
023400980131     O                                         +  1 '---------'
023500980131     O                                         +  1 '---------'
023600980131     O                                         +  1 '---------'
023700010903     O          e            hedr           1
023800010903     O         or          of
023900010903     O                                           50 'RETURNED '
024000980817     O                                         +  2 ' PAIDIN  '
024100980817     O                                         +  2 ' NUMBER  '
024200010903     O                                           81 'SIGNITURE'
024300010903     OQSYSPRT   E            dept_hdr    2  1
024400010903     O                       dept          1      6
024500010903     O                       dname               47
024600010903     OQSYSPRT   E            details     1
024700010903     O                       run_name            27
024800010903     O                       amount        1     35
024900010903     O                                         +  6 '_________'
025000010903     O                                         +  2 '_________'
025100010903     O                                         +  2 '________'
025200010903     O                                         +  1 '_________'
025300980131     OQSYSPRT   E            GTOT        3
025400980817     O                                           10 '  TOTALS '
025500010904     OQSYSPRT   E            TOTREC      1
025600010904     O                       run_name            25
025700010904     O                       run_total     z     35
025800980131     OQSYSPRT   E            REPEND      3  1
025900980127     O                                          115 'PAID BY:__________________'
026000980131     OQSYSPRT   E            REPEND      1  1
026100980131     O                                          115 'PAID BY:__________________'
026200980704     OQSYSPRT   E            REPEND      1  1
026300980704     O                                          115 'CHECKED BY:_______________'
026400971231     OQSYSPRT   E            REPEND      2  1
026500971231     O                                           65 '*** END OF REPORT ***'
026600010903**
026700010904 Breakdown of Payslips for period
026800010905          Payroll Check Sheet

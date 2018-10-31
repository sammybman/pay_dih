000100941121     H DATEDIT(*DMY)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR':'PAYDIR')
000300111002     H/define profoundui
000400990209     **************************************************
000500111002     ** PAYROLL RUN TYPE MAINTENANCE PROGRAM  **
000600990209     **************************************************
000700111002     Fpayrtypl01uf a e           k disk    usropn
000800111002     Fpaycycd   if   e           k disk    usropn
000900020613     **
001000020613     Fpayrdefl01uf   e           k disk    usropn
001100020613     Fpayrresl01uf   e           k disk    usropn
001200020613     Fpaytranl06uf   e           k disk    usropn
001300020613     Fpayhisl09 uf   e           k disk    usropn
001400040621     Fpayrunt   uf   e           k disk    usropn
001500020613     **
001600010425     Fpaymnt05d cf   e             workstn
001700111002     F/IF DEFINED(WEB)
001800111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001900111002     F/ENDIF
002000010508     ** Payroll PROTOTYPES
002100110901      /copy QCPYSRC,genPR
002200110901      /copy QCPYSRC,PAYPR
002300110901      /copy QCPYSRC,autPR
002400990112     **
002500010815     D@date            s             10
002600010815     Ddate             s              8  0
002700040621     D arr             S             40    dim(1) CTDATA PERRCD(1)
002800040621
002900010815     D                sds
003000990211     D  @pgm             *proc
003100111002     D  @user                358    367
003200040621
003300111002     Dset_prd_per      pr                  extpgm('PAYMNT0501')
003400040621     D run_type                       1    const
003500040621     D no_Of_Prd                      2  0 const
003600010425     **
003700010424     D  msg1           c                   const('Description cannot be Blanks')
003800010425     D  msg2           c                   const('Last Run Date is Invalid')
003900010425     D  msg3           c                   const('Next Run Date has already +
004000010425     D                                            past')
004100011102     D  msg4           c                   'Must enter Cycle Code'
004200011102     D  msg5           c                   'Must enter Cycle Quantity'
004300011102     D  msg6           c                   'No. of Periods and Cycle info. conf-
004400011102     D                                     lict'
004500020613     D  msg7           c                   'Delete Request cancelled'
004600020613     D  msg8           c                   'Deletion completed successfully'
004700040621
004800950711     **
004900010424     C                   move      *blanks       #mode
005000040207      /free
005100111002         // authority check
005200111002         monitor;
005300111002           authority_check('PAY':@pgm);
005400111002         on-error;
005500111002           show_msg(get_err_msg('DIH0054'));
005600111002           *inlr = *on;
005700111002           return;
005800111002         endmon;
005900040207      /end-free
006000010424     ** Initial Setup
006100010424     C                   exsr      aainit
006200010424     ************************************
006300010424     ** First screen to request number **
006400010424     ************************************
006500010424     C                   dow       0 = 0
006600941123     **
006700010424     C                   exfmt     fmt1
006800941123     **
006900010424     C                   if        *in03
007000010424     C                   eval      *inlr = *on
007100010424     C                   return
007200010424     C                   endif
007300010424
007400010917     C                   if        *in01
007500010509     C                   call      'PAYENQ02R'
007600010509     C                   parm                    prtyptype#
007700010509     C                   iter
007800010509     C                   endif
007900010424     **
008000010425     C     prtyptype#    chain     prtyfmt                            80
008100010424     C                   if        *in80
008200010425     C                   move      ' ADD  '      #mode
008300010425     C                   clear                   prtyfmt
008400010425     C                   eval      prtyldate = date
008500010425     ** setup mode description
008600010424     C                   else
008700010430     C                   move      'AMEND '      #mode
008800010424     C                   endif
008900010424
009000010424     C                   leave
009100010424     C                   enddo
009200941121     **
009300010424     **********************
009400010424     ** Body of Program  **
009500010424     **********************
009600010424     C                   dow       0 = 0
009700941121     **
009800010430     C                   exfmt     fmt2
009900990209
010000010424     ** clear error Field
010100010424     C                   eval      errmsg = *blanks
010200010424     ** End Program
010300010424     C                   if        *in03
010400010424     C                   eval      *inlr = *on
010500010424     C                   return
010600010424     C                   endif
010700010424     ** Cancel selection
010800010424     C                   if        *in02
010900010424     C                   leave
011000010424     C                   endif
011100020613     ** Delete routine
011200020613     C                   if        *in04
011300020613     C                   eval      keep = 'N'
011400020613     C                   eval      delete = 'N'
011500020613     C                   exsr      delete_sr
011600020613     C                   leave
011700020613     C                   endif
011800010904     ** Cycle Code List
011900010917     C                   if        *in01
012000010904     C                   call      'PAYENQ04R'
012100010904     C                   parm                    prtycycode
012200010904     C                   iter
012300010904     C                   endif
012400040621
012500040621      /free
012600040621        // Set Period Payment Percentage
012700040621          if  *in09;
012800040621            set_prd_per(prtyptype#:prtyprno);
012900040621            iter;
013000040621          endif;
013100040621      /end-free
013200010430     ** Validate Description
013300010425     C                   if        prtydesc = *blanks
013400010424     C                   eval      errmsg = msg1
013500010424     C                   iter
013600010424     C                   endif
013700010425     ** validate last date
013800010425     C     *iso          test(d)                 prtyldate              70
013900010425     C                   if        *in70
014000010425     C                   eval      errmsg = msg2
014100010425     C                   iter
014200010425     C                   endif
014300010425     ** validate next date
014400010430     C                   if        prtyprno > 0
014500010430     ** Validate Cycle code
014600010430     C     prtycycode    chain     cycfmt
014700010430     C                   if        not%found
014800010430     C                   eval      errmsg = msg4
014900010430     C                   iter
015000010430     C                   endif
015100010430     ** Validate Cycle qty
015200010430     C                   if        prtycycqty <= 0
015300010430     C                   eval      errmsg = msg5
015400010430     C                   iter
015500010430     C                   endif
015600010430     **
015700010508     C                   eval      prtyndate = get_next_date(prtyldate:
015800010508     C                             cyccode:prtycycqty:cycccode:
015900010508     C                             cycconvf)
016000010425     C                   if        prtyndate < date
016100010425     C                   eval      errmsg = msg3
016200010430     C                   iter
016300010430     C                   endif
016400011102     ** check periods against cycle
016500011102     C                   if        prtyprno > (cycyearf /prtycycqty)
016600011102     C                   eval      errmsg = msg6
016700011102     C                   iter
016800011102     C                   endif
016900010430     C                   endif
017000010425     ** Confirm record
017100010425     C                   if        *in15
017200010425     C                   exsr      confirm_sr
017300010917     C                   leave
017400010425     C                   endif
017500010424     **
017600010424     C                   enddo
017700010425     **
017800111002      /free
017900111002
018000111002         //*****************************
018100111002         // Session Init
018200111002         //*****************************
018300111002
018400111002          begsr *inzsr;
018500111006            get_session_info('PAY':errmsg:@pgm);
018600111002            if errmsg <> *blanks;
018700111002              show_msg(errmsg);
018800111002              *inlr = *on;
018900111002              return;
019000111002            endif;
019100111002
019200111002            open(e) payrtypl01;
019300111002            open(e) paycycd;
019400111002
019500111002          endsr;
019600111002
019700111002      /end-free
019800111002
019900010425     *************************
020000010425     ** Write/Update Record **
020100010425     *************************
020200010425     C     confirm_sr    begsr
020300010425     ** Write/Update Record
020400010425     C                   if        *in80
020500010425     ** Present Confirm Screen
020600010425     C                   exfmt     win1
020700010425     **
020800010425     C                   if        confirm = 'Y'
020900020624     ** Set Run Flag
021000020624     C                   if        prtyprno = *zeros
021100020624     C                   eval      prtyflag = 'Y'
021200020624     C                   else
021300020624     C                   eval      prtyflag = *blank
021400020624     C                   endif
021500010425     C                   eval      prtysts = 'A'
021600010720     C                   eval      prtyyear = *year
021700010425     C                   eval      prtyptype = prtyptype#
021800010425     C                   write     prtyfmt
021900010425     C                   endif
022000010425     **
022100010425     C                   else
022200010425     C                   update    prtyfmt
022300010425     C                   endif
022400010425     C                   endsr
022500020613     *******************
022600020613     ** Delete Record **
022700020613     *******************
022800020613     C     delete_sr     begsr
022900020613
023000020613     ** Present Confirm Screen
023100020613     C                   exfmt     win2
023200020613     **
023300020613     C                   if        delete = 'Y'
023400020613     ** Delete all associated transactions
023500020613     C                   if        keep <> 'Y'
023600020613     ** run type defaults
023700020613     C                   open      payrdefl01
023800020613     C     prtyptype#    setll     prdffmt
023900020613     C     prtyptype#    reade     prdffmt
024000020613     C                   dow       not(%eof)
024100020613     C                   delete    prdffmt
024200020613     C     prtyptype#    reade     prdffmt
024300020613     C                   enddo
024400020613     C                   close     payrdefl01
024500020613     ** run type restrictions
024600020613     C                   open      payrresl01
024700020613     C     prtyptype#    setll     prrefmt
024800020613     C     prtyptype#    reade     prrefmt
024900020613     C                   dow       not(%eof)
025000020613     C                   delete    prrefmt
025100020613     C     prtyptype#    reade     prrefmt
025200020613     C                   enddo
025300020613     C                   close     payrresl01
025400020613     ** Transaction master
025500020613     C                   open      paytranl06
025600020613     C     prtyptype#    setll     ptisfmt
025700020613     C     prtyptype#    reade     ptisfmt
025800020613     C                   dow       not(%eof)
025900020613     C                   delete    ptisfmt
026000020613     C     prtyptype#    reade     ptisfmt
026100020613     C                   enddo
026200020613     C                   close     paytranl06
026300020613     ** History Master
026400020613     C                   open      payhisl09
026500020613     C     prtyptype#    setll     phisfmt
026600020613     C     prtyptype#    reade     phisfmt
026700020613     C                   dow       not(%eof)
026800020613     C                   delete    phisfmt
026900020613     C     prtyptype#    reade     phisfmt
027000020613     C                   enddo
027100020613     C                   close     payhisl09
027200040621     ** Run type period percentage file
027300040621     C                   open      payrunt
027400040621     C     prtyptype#    setll     prunfmt
027500040621     C     prtyptype#    reade     prunfmt
027600040621     C                   dow       not(%eof)
027700040621     C                   delete    prunfmt
027800040621     C     prtyptype#    reade     prunfmt
027900040621     C                   enddo
028000040621     C                   close     payrunt
028100020613     **
028200020613     C                   endif
028300020613
028400020613     C                   delete    prtyfmt
028500020613     C                   eval      errmsg = msg8
028600020613     **
028700020613     C                   else
028800020613     C                   eval      errmsg = msg7
028900020613     C                   endif
029000020613     C                   endsr
029100941120     **
029200010424     ***************************************************
029300010424     ** First initilisation routine for all programs  **
029400010424     ***************************************************
029500010424     C     AAINIT        BEGSR
029600010815     C                   eval      CONAME = get_coname
029700010815     C                   eval      @DATE = get_sys_date(date)
029800010424     C                   time                    @TIME             6 0
029900010424     ** select header for program
030000010424     C                   movel(p)  ARR(1)        @TITLE
030100010424     **
030200010424     C                   ENDSR
030300010424     **
030400010424**  Program Title
030500010425      Payroll Run Type Maintenance

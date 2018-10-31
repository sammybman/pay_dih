000100091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000200111002     H/define profoundui
000300010917     Fpaymnt11d cf   e             WORKSTN
000400021031     F                                     SFILE(sflrec:rrn1)
000500111002     F/IF DEFINED(WEB)
000600111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
000700111002     F/ENDIF
000800111002     Fpayeytd   uf   e           k disk    usropn
000900021031     Fqsysprt   o    f  132        printer
001000010917     ** Payroll/general PROTOTYPES
001100111002      /copy QCPYSRC,genpr
001200111002      /copy QCPYSRC,PAYPR
001300111002      /copy QCPYSRC,autpr
001400111002      /copy QCPYSRC,emppr
001500010917
001600010917     D                sds
001700010917     D  @pgm             *proc
001800111002     D  @user                358    367
001900981222     **
002000010917     D finds           ds                  occurs(99) inz
002100010917     D  fingro                       13  2
002200010917     D  fintax                       13  2
002300010917     D  finntx                        9  2
002400010917     D  finmed                        9  2
002500010917     D  finsav                       11  2
002600010917     D  count                         4  0
002700010917     **
002800010917     D arr             s             40    dim(1) ctdata perrcd(1)
002900021031     D stat_arr        s              1    dim(99)                              MVEMNT DESCRIPT.
003000021031     D rrn1            s              3  0                                      MVEMNT DESCRIPT.
003100021031     D xcnt            s                   like(rrn1)                           MVEMNT DESCRIPT.
003200021031     D ycnt            s                   like(rrn1) inz(1)                    MVEMNT DESCRIPT.
003300010917     **
003400010917     D  txt            s             20
003500010917     D  acnt           s              2  0
003600040224     D* status         s              1
003700010917     D  date           s              8  0
003800010917     D  @year          s                   like(year)
003900010917     **
004000010917     D  msg1           c                   'YTD record not found'
004100010917     D  msg2           c                   'Cannot Change This Record'
004200021031     D  plus           c                   '+'
004300021031     D  minus          c                   '-'
004400021031     D  change         c                   'C'
004500990403     **
004600990403
004700040207      /free
004800111002         // authority check
004900111002         monitor;
005000111002           authority_check('PAY':@pgm);
005100111002         on-error;
005200111002           show_msg(get_err_msg('DIH0054'));
005300111002           *inlr = *on;
005400111002           return;
005500111002         endmon;
005600040207      /end-free
005700950802     **
005800010917     C     trn_key       klist
005900010917     C                   kfld                    emp
006000010917     C                   kfld                    year
006100021031     **
006200021031     C     trn_key2      klist
006300021031     C                   kfld                    emp
006400021031     C                   kfld                    year
006500021031     C                   kfld                    ytdtyp#
006600021031     C                   kfld                    ytdopt#
006700010917
006800010917     C                   exsr      aainit
006900021031     C                   movel     date          @year
007000010917     ********************
007100010917     ** Select Record  **
007200010917     ********************
007300010917     C                   dow       0 = 0
007400010917     **
007500010917     C                   exfmt     fmt1
007600010917     C                   eval      errmsg = *blanks
007700010917     ** End Program
007800010917     C                   if        *in03
007900010917     C                   eval      *inlr = *on
008000010917     C                   exsr      generate_rtn
008100010917     C                   return
008200010917     C                   endif
008300021031
008400021031     C     trn_key       chain     ytdrec
008500021031     **
008600021031     C                   if        not(%found)
008700021031     C                   eval      errmsg = msg1
008800021031     C                   iter
008900021031     C                   endif
009000021031     **
009100021031     C                   if        year < (@year - 1)
009200021031     C                   eval      errmsg = msg2
009300021031     C                   iter
009400021031     C                   endif
009500021031     **
009600021031     C                   exsr      fill_subfile
009700021031     C                   eval      status = get_emp_info(emp:*omit:*blanks
009800021031     C                                      :emp_name)
009900021031
010000021031     C                   leave
010100021031     C                   enddo
010200950802     **
010300021031
010400950802     **
010500021031     ******************
010600021031     ** Calcs Section *
010700021031     ******************
010800021031     C                   eval      *in75 = rrn1 > *zeros
010900021031     C                   write     footer
011000021031     ** display records only for selected grade
011100021031     c                   dow       0 = 0
011200021031     C                   exfmt     sflctl
011300021031     ** end program
011400021031     C                   if        *in03
011500021031     C                   eval      *inlr = *on
011600021031     C                   exsr      generate_rtn
011700021031     C                   return
011800021031     C                   endif
011900021031     C
012000021031     ** Refresh Records
012100021031     c
012200021031     C                   if        *in05
012300021031     C                   exsr      refresh_lines
012400021031     C                   iter
012500021031     C                   endif
012600021031     c
012700021031     ** UPdate Transactions
012800021031     C                   exsr      update_lines
012900021031     **
013000021031     C
013100021031     ** F15 = Confirm changes
013200021031     C                   if        *in15
013300021031     C                   exsr      confirm_sr
013400021031     C                   leave
013500021031     C                   endif
013600021031     C
013700021031     C                   enddo
013800021031
013900021031      /free
014000111002
014100111002         //*****************************
014200111002         // Session Init
014300111002         //*****************************
014400111002
014500111002          begsr *inzsr;
014600111006            get_session_info('PAY':errmsg:@pgm);
014700111002            if errmsg <> *blanks;
014800111002              show_msg(errmsg);
014900111002              *inlr = *on;
015000111002              return;
015100111002            endif;
015200111002
015300111002            open(e) payeytd;
015400111002
015500111002          endsr;
015600111002
015700021031       //*******************************
015800021031       //** Confirm changes made
015900021031       //*******************************
016000021031
016100021031        begsr confirm_sr;
016200021031           xcnt = rrn1;
016300021031           reset ycnt;
016400021031           for ycnt to xcnt;
016500021031           chain ycnt sflrec;
016600021031           chain trn_key2 ytdrec;
016700021031
016800021031            if ytdamt# <> newytd;
016900021031               ytdamt = newytd;
017000021031               update ytdrec;
017100030209           // Taxable Gross (type_flag = Y)
017200030209               if  type_flag = 'Y';
017300030209
017400030209                 ytdtyp# = *zeros;
017500030209                 chain trn_key2 ytdrec;
017600030209                 ytdamt = ytdamt - ytdamt# + newytd;
017700030209                 if  %found(payeytd);
017800030209                  update  ytdrec;
017900030209                 endif;
018000030209               endif;
018100021031            endif;
018200021031
018300021031           endfor;
018400021031
018500021031        endsr;
018600021031
018700021031       //*******************************
018800021031       //** UPdate Lines
018900021031       //*******************************
019000021031
019100021031        begsr update_lines;
019200021031           xcnt = rrn1;
019300021031           reset ycnt;
019400021031           for ycnt to xcnt;
019500021031           chain ycnt sflrec;
019600021031
019700021031
019800021031            select;
019900021031            // adding to YTD.
020000021031             when sel = plus;
020100021031              newytd = newytd + chgamt;
020200021031            // subtracting from ytd
020300021031             when sel = minus;
020400021031              newytd = newytd - chgamt;
020500021031
020600021031            // subtracting from ytd
020700021031             when sel = minus;
020800021031              newytd = newytd - chgamt;
020900021031
021000021031            // Changing Ytdd
021100021031             when sel = change;
021200021031              newytd = chgamt;
021300021031             endsl;
021400021031
021500021031            // Round up all cents to dollars
021600021031             newytd = round_up(newytd:'A');
021700021031              chgamt = *zeros;
021800021031              sel = *blank;
021900021031
022000021031             update  sflrec;
022100021031
022200021031           endfor;
022300021031        endsr;
022400021031
022500021031       //*******************************
022600021031       //** Refresh lines
022700021031       //*******************************
022800021031
022900021031        begsr refresh_lines;
023000021031           xcnt = rrn1;
023100021031           reset ycnt;
023200021031           for ycnt to xcnt;
023300021031           chain ycnt sflrec;
023400021031
023500021031              newytd = ytdamt#;
023600021031              chgamt = *zeros;
023700021031              sel = *blank;
023800021031
023900021031             update  sflrec;
024000021031
024100021031           endfor;
024200021031        endsr;
024300021031
024400021031       //***********************
024500021031       //** Get Lines         **
024600021031       //***********************
024700021031        begsr fill_subfile;
024800021031          //   Clear Sub_file
024900021031             *in33 = *on;
025000021031             rrn1 = *zeros;
025100021031             write  sflctl;
025200021031             *in33 = *off;
025300021031
025400021031          setll  trn_key ytdrec;
025500021031          reade  trn_key ytdrec;
025600021031          dow   not(%eof);
025700030209              if  ytdtype <> *zeros;
025800021031          // Screen fields
025900021031             newytd = ytdamt;
026000021031             ytdorg# = ytdorg;
026100021031             ytdopt# = ytdopt;
026200021031             ytdamt# = ytdamt;
026300021031             ytdtyp# = ytdtype;
026400021031
026500021031          //  Get Type Description
026600050524                get_payty_info('RD':ytdtype:payty_struc);
026700050524
026800030209                type_flag = ptytcode;
026900021031
027000021031             rrn1 = rrn1 + 1;
027100021031             write   sflrec;
027200030209              endif;
027300021031
027400021031           reade trn_key ytdrec;
027500021031          enddo;
027600021031             *in55 = rrn1 > *zeros;
027700021031        endsr;
027800021031
027900021031       //***************************************************
028000021031       //** First initilisation routine for all programs  **
028100021031       //***************************************************
028200021031        begsr aainit;
028300021031           @title = arr(1);
028400021031           coname = get_coname;
028500021031           @date = get_sys_date(date);
028600021031         //  @year = date;
028700021031
028800021031        endsr;
028900021031      /end-free
029000010917     **
029100010917     **************************************
029200010917     *** GENERATE REPORT AT PROGRAM END ***
029300010917     **************************************
029400010917     C     generate_rtn  begsr
029500021031     C*                  do        xcnt          acnt
029600021031     C*    acnt          occur     finds
029700010917     **
029800021031     C*                  callp     get_cat(stat_arr(acnt):txt)
029900010917     **
030000021031     C*                  if        count <> *zeros
030100021031     C* N88              EXCEPT    HEDR
030200021031     C* N88              SETON                                        88
030300021031     C*                  EXCEPT    DETAIL
030400021031     C*                  ENDIF
030500010917     **
030600021031     C*                  ENDDO
030700021031     C                   endsr
030800021031     Oqsysprt   E            HEDR1          1 02
030900970806     O                       @PGM                10
031000021031     O                       coname              74
031100950901     O                       UDATE         Y    124
031200950901     O          E            HEDR1          2
031300970806     O                       @USER               10
031400950901     O          E            HEDR1       0  1
031500950901     O                                           67 'EMPLOYEES Y.T.D'
031600950901     O                                         +  1 'CHANGES'
031700950901     O                                          120 'PAGE'
031800950901     O                       PAGE              +  1
031900950901     O          E            HEDR1       0  1
032000950902     O                                           67 '==============='
032100950902     O                                         +  0 '========'
032200950901     O          E            HEDR1       1  1
032300010917     O                                            5 'YEAR'
032400950901     O                                           16 'EMP. NUM'
032500950901     O                                           37 'GROSS'
032600950901     O                                           56 'TAX'
032700950901     O                                           73 'N.I.S.'
032800950901     O                                           89 'MEDICAL'
032900950901     O                                          106 'SAVINGS'
033000950901     O          E            HEDR1       0  1
033100010917     O                                            5 '----'
033200950901     O                                           16 '--------'
033300950901     O                                           37 '-----'
033400950901     O                                           56 '---'
033500950901     O                                           73 '------'
033600950901     O                                           89 '-------'
033700950901     O                                          106 '-------'
033800010917**
033900010917        Employee YTD. Movement

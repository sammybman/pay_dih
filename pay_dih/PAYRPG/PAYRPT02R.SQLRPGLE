000100950808     H DATEDIT(*DMY)
000200091119     H dftactgrp(*no) actgrp(*caller) bnddir('DISDIR')
000300180117     H alwnull(*usrctl)
000400111002     H/define profoundui
000500960713     F** EMPLOYEE'S INCOME TAX *********
000600960713     F** Programmer S.BRITTON
000700111002     Fmasall    if   e           k disk    usropn
000800111002     Fpayeytd   if   e           k disk    usropn
000900111002     Fpaytaxctl if   e           k disk    usropn
001000050422     Fpayhisl05 if   e           k disk    usropn
001100111002     Fpaycon    if   e           k disk    usropn
001200111002     Fpayopt    if   e           k disk    usropn
001300011008     Fpayrpt02d cf   e             workstn
001400111002     F/IF DEFINED(WEB)
001500111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
001600111002     F/ENDIF
001700011008     Fqsysprt   O    f  132        printer oflind(*inof)
001800011008     ** Payroll PROTOTYPES
001900111002      /copy QCPYSRC,PAYPR
002000111002      /copy QCPYSRC,genPR
002100111002      /copy QCPYSRC,autPR
002200050422     D
002300050422     D*-------------------------------------
002400050422     D* Prototype for procedure: getStrDate
002500050422     D*-------------------------------------
002600050422     D getStrDate      PR              d
002700050422     D
002800050422
002900011012     D                SDS
003000011012     D  @PGM             *PROC
003100111002     D  @USER                358    367
003200021101     **
003300010109     Dyrend_date       c                   const('31.12.')
003400010109     Dyrstr_date       c                   const('01.01.')
003500011008     D msg1            c                   'A Blank Entry Not Allowed'
003600011008     D msg2            c                   'Invalid Employee Type '
003700011008     D msg3            c                   'Report sent to print queue'
003800021101     D salary_text     c                   'SALARY'
003900021101     **
004000021101     D arr             S             40    dim(1) ctdata perrcd(1)
004100010109     Deur_date_emp     s               d   datfmt(*eur)
004200010109     Deur_date_str     s               d   datfmt(*eur)
004300010109     Dstart_period     s               d   datfmt(*eur)
004400010109     Dend_period       s               d   datfmt(*eur)
004500041025     Demp_date_str     s             10
004600010109     Dchr_date_str     s             10
004700010109     Dchr_date_end     s             10
004800021101     DlineDesc         s             40
004900021101     Dytdtot           s                   like(ytdamt)
005000011008     Dnew_year         s                   like(pay_year)
005100021101     Dgro_type         s              2  0 inz(00)
005200021101     Dtax_type         s              2  0 inz(61)
005300150824     Dnis_type         s              2  0 inz(63)
005400011008     Dname             s             25
005500011008     Dsex1             s              5
005600180118
005700180118       // --------------------------------------------------
005800180118       // Prototype for procedure: getNISValuePrc
005900180118       // --------------------------------------------------
006000180118       DCL-PR getNISValuePrc PACKED(11:2);
006100180118         emp_in PACKED(6:0);
006200180118         income_year PACKED(4:0);
006300180118         NIS_Code PACKED(2:0);
006400180118       END-PR ;
006500180118
006600180118       //
006700180118       DCL-S nis_ytdamt PACKED(11:2) inz(0) ;
006800180118       //
006900180118       DCL-S ytdamt# LIKE(ytdamt) ;
007000180118       //
007100180118       DCL-S ytdtot# LIKE(ytdtot) INZ(0);
007200180118       //
007300180118       DCL-S YTDTOTNIS LIKE(YTDTOT) INZ(0);
007400180118     D error           s               n
007500010906     **
007600040207      /free
007700111002         // authority check
007800111002         monitor;
007900111002           authority_check('PAY':@pgm);
008000111002         on-error;
008100111002           show_msg(get_err_msg('DIH0054'));
008200111002           *inlr = *on;
008300111002           return;
008400111002         endmon;
008500040207      /end-free
008600011008     C                   exsr      aainit
008700011008     C                   dow       0 = 0
008800011008     C                   exfmt     fmt1
008900011008     ** reset error tags
009000011008     C                   clear                   errmsg
009100011008     C                   eval      *in28 = '0'
009200011008     C                   eval      *in29 = '0'
009300011008
009400011008     C                   if        *in03
009500011008     C                   eval      *inlr = '1'
009600011008     C                   return
009700011008     C                   endif
009800011008
009900011008     ** Employee Type List
010000011008     C                   if        *in01
010100011008     C                   call      'HMRENQ08R'
010200011008     C                   parm                    emp_type
010300011008     C                   iter
010400011008     C                   endif
010500011008     ** Validate year entry
010600011008     C                   if        pay_year <= 0
010700011008     C                   eval      *in28 = '1'
010800011008     C                   eval      errmsg = msg1
010900011008     C                   iter
011000011008     C                   endif
011100011008     ** Validate employee type
011200011008     C                   callp     get_cat(emp_type:*omit:*omit:*omit
011300011008     C                                     :error)
011400011008     C                   if        error
011500011008     C                   eval      *in29 = '1'
011600011008     C                   eval      errmsg = msg2
011700011008     C                   iter
011800011008     C                   endif
011900011008
012000011008     C                   eval      new_year = pay_year + 1
012100011008     C                   leave
012200011008     C                   enddo
012300950816     **
012400021101     *********************************
012500021101     ** GENERATE 7B2 LETTERS
012600021101     *********************************
012700020326     C     '*Y'          chain     ptxfmt
012800020326     **
012900021101     C     gro_key       klist
013000010906     C                   kfld                    emp
013100011008     C                   kfld                    pay_year
013200021101     C                   kfld                    gro_type
013300021101     **
013400021101     C     tax_key       klist
013500021101     C                   kfld                    emp
013600021101     C                   kfld                    pay_year
013700021101     C                   kfld                    tax_type
013800151117     **
013900151117     C     nis_key       klist
014000151117     C                   kfld                    emp
014100151117     C                   kfld                    pay_year
014200151117     C                   kfld                    nis_type
014300011008
014400011008     C     *loval        setll     emprec
014500011008     C                   read      emprec
014600011008     C                   dow       not(%eof)
014700950816     **
014800011008     C                   eval      name = %trim(christ) + ' '
014900011008     C                                  + %triml(surnam)
015000021101
015100021101     C                   if        status = emp_type
015200950816     **
015300021101     C     gro_key       setll     ytdrec
015400021101     C     gro_key       reade     ytdrec
015500950816     **
015600021101     C                   if        not(%eof(payeytd))
015700021101     **
015800021101     C                   exsr      get_hdr_Data
015900021101     **
016000021101     c                   except    reg
016100021101     ** Print details (Taxable Gross)
016200021101     C                   dow       not(%eof(payeytd))
016300021101     **        get description for other
016400180118                   reset nis_ytdamt;
016500180118                      if        ytdopt = *zeros;
016600180118                         linedesc = salary_text;
016700180118                         nis_ytdamt = getNISValuePrc(emp:pay_year:nis_type);
016800180131                       //YTDAMT# =   YTDAMT + nis_ytdamt;
016900180131                       //YTDTOT# = YTDTOT#+ nis_ytdamt;
017000180131                         YTDAMT# =   YTDAMT;
017100180131                         YTDTOT# = YTDTOT#;
017200180118                        else;
017300021101     C     ytdopt        chain     pytdfmt
017400021101     C                   eval      linedesc = pytddesc
017500180118                      endif;
017600950816     **
017700180131                         ytdtot += ytdamt;
017800180131                         YTDTOT# += ytdamt;
017900180118                         if nis_ytdamt > 0;
018000180118                          ytdamt = ytdamt#;
018100180118                          reset nis_ytdamt;
018200180118                          reset ytdamt#;
018300180118                         ENDIF;
018400021101     C                   except    regdet
018500950816     **
018600021101     C     gro_key       reade     ytdrec
018700021101     C                   enddo
018800021101     ** Print Total Earnings
018900021101     C                   except    regTOT
019000180118        //---------------------------------------------------
019100180118         //** Print details (NIS Deductions)
019200180118        //---------------------------------------------------
019300021101
019400180118                         YTDTOTNIS = getNISValuePrc(emp:pay_year:nis_type);
019500180131                         YTDTOT  -=  ytdtotNIS;
019600180118
019700180118     C                   except    regTOT3
019800180118        //---------------------------------------------------
019900180118         //** Print details (Taxable Earnings
020000180118        //---------------------------------------------------
020100180118     C                   except    regTOT1
020200180118        //---------------------------------------------------
020300021101     ** Print details (Income Tax)
020400180118        //---------------------------------------------------
020500021101     C     tax_key       setll     ytdrec
020600021101     C     tax_key       reade     ytdrec
020700021101     C                   dow       not(%eof(payeytd))
020800021101     **
020900021101     C                   eval      ytdtot = ytdtot + ytdamt
021000021101     C     tax_key       reade     ytdrec
021100021101     C                   enddo
021200021101     C                   except    regTOT2
021300151117
021400180118     ***
021500021101     C                   except    regFIN
021600021101     C                   endif
021700950816     **
021800011008     C                   endif
021900011008     C                   read      emprec
022000011008     C                   enddo
022100011008     C                   eval      errmsg = msg3
022200011008     ***************************************************
022300011008     ** First initilisation routine for all programs  **
022400011008     ***************************************************
022500011008     C     aainit        Begsr
022600011008     C                   eval      @title = arr(1)
022700011008     C                   eval      coname = get_coname
022800011008     C                   eval      @date = get_sys_date
022900011008     **
023000011008     C                   Endsr
023100021101
023200021101     ************************
023300021101     ** Get Report Heading **
023400021101     ************************
023500021101     C     get_hdr_data  begsr
023600021101     C                   eval      *in10 = sex = 'M'
023700021101     C                   eval      *in11 = sex = 'F'
023800021101     C   10              eval      sex1 = '  SIR'
023900021101     C   11              eval      sex1 = 'MADAM'
024000021101     ** Get Report period
024100021101     C                   eval      chr_date_str = yrstr_date +
024200021101     C                                            %char(pay_year)
024300021101     C                   eval      chr_date_end = yrend_date +
024400021101     C                                            %char(pay_year)
024500021101     C                   move      chr_date_str  eur_date_str
024600021101     ** Get date employed
024700041107     C                   if        pctxprate = 'Y'
024800041107
024900041025     C                   if        %date(emply:*eur) < eur_date_str
025000041025     C                   move      chr_date_str  eur_date_emp
025100041025     C                   else
025200050422      /free
025300050422          eur_date_emp = getStrDate();
025400050422      /end-free
025500041025     C                   endif
025600041107
025700041107     C                   else
025800041107     C                   move      chr_date_str  eur_date_emp
025900041107     C                   endif
026000021101     **
026100021101     ** Get date resigned for period end date
026200021101     C                   if        Resign <> *zeros
026300021101     C                   move      resign        end_period
026400021101     C                   else
026500021101     C                   move      chr_date_end  end_period
026600021101     C                   endif
026700021101     **
026800021101     C                   if        eur_date_str > eur_date_emp
026900021101     C                   eval      start_period = eur_date_str
027000021101     C                   else
027100021101     C                   eval      start_period = eur_date_emp
027200021101     C                   endif
027300021101     C                   endsr
027400041107
027500041107     C     *inzsr        begsr
027600041107      /free
027700111006            get_session_info('PAY':errmsg:@pgm);
027800111002            if errmsg <> *blanks;
027900111002              show_msg(errmsg);
028000111002              *inlr = *on;
028100111002              return;
028200111002            endif;
028300111002
028400111002            open(e) masall;
028500111002            open(e) payeytd;
028600111002            open(e) paytaxctl;
028700111002            open(e) payopt;
028800111002
028900111002            open(e) paycon;
029000041107          read pconfmt;
029100041107          close(e) paycon;
029200041107      /end-free
029300041107     C                   endsr
029400020326
029500011008     Oqsysprt   e            reg            1 06
029600980903     O                                           11 'NAME:'
029700011008     O                       surnam         B    25
029800980903     O                                           26 ','
029900011008     O                       christ         B    39
030000980903     O                                           54 'EMP. NO.:'
030100021101     O                       emp           Z     59
030200011008     O          e            reg            2
030300980903     O                                           11 'DATE:'
030400011008     O                       udate         Y     20
030500980903     O                                           54 'DEPT.NO.:'
030600011008     O                       dept          ZB    59
030700011008     O          e            reg            2
030800980903     O                                            6 'DEAR'
030900011008     O                     10sex1                12
031000011008     O                     11sex1                12
031100980903     O                                           13 ','
031200980903     O              N10N11                       14 'MR./MS.'
031300980903     O              N10N11                       15 ','
031400980903     O          E            reg            1
031500980903     O                                           26 'PLEASE NOTE THAT WE HAVE'
031600980903     O                                           52 'ADVISED THE COMMISSIONER'
031700980903     O                                           55 'OF'
031800980903     O          E            reg            1
031900980903     O                                           26 'INLAND REVENUE OF SALARI'
032000980903     O                                           48 'ES PAID TO AND INCOME '
032100980903     O                                           51 'TAX'
032200011008     O          e            reg            1
032300980903     O                                           24 'DEDUCTED FROM YOU FOR '
032400010109     O                                           35 'THE PERIOD:'
032500010109     O                       start_period        46
032600010109     O                                           49 'TO'
032700010109     O                       end_period          60
032800010109     O                                           61 '.'
032900011008     O          e            reg            3
033000980903     O                                           13 'AS FOLLOWS:'
033100021101     O          e            regDET         2
033200021101     O                       linedesc            42
033300980903     O                                           45 '$'
033400021101     O                       ytdamt        1B    58
033500021101     O          e            regTOT         2
033600180118     O                                           19 '** TOTAL EARNINGS'
033700180118     O                       YTDTOT#       1B    58
033800980903     O                                           62 '***'
033900180118     O          e            regTOT1        2
034000180118     O                                           21 '** TAXABLE EARNINGS'
034100180118     O                       YTDTOT        1B    58
034200180118     O                                           62 '***'
034300021101     O          e            regTOT2        2
034400980903     O                                           21 'INCOME TAX DEDUCTED'
034500021101     O                       ytdtot        1B    58
034600151117
034700151117     O          e            regTOT3        2
034800160113     O                                           19 'N.I.S. DEDUCTIONS'
034900180118     O                       ytdtotnis     1B    58
035000060207     O          e            regfin         1
035100980903     O                                           23 'IF YOUR TOTAL INCOME '
035200980903     O                                           46 'FROM ALL SOURCES INCLUD'
035300980903     O                                           53 'ING THE'
035400021101     O          e            regfin         1
035500020326     O                                           15 'ABOVE IS OVER'
035600020326     O                       ptxfree       1  +   1 '$'
035700020326     O                                        +   1 'YOU ARE REQUIRED UNDER '
035800020326     O                                        +   1 'PENALTY TO'
035900021101     O          e            regfin         1
036000980903     O                                           26 'MAKE A RETURN ON FORM 1 '
036100980903     O                                           50 'ATTACHED TO THE COMMISS'
036200980903     O                                           55 'IONER'
036300021101     O          e            regfin         2
036400980903     O                                           21 'OF INLAND REVENUE, '
036500980903     O                                           45 'GEORGETOWN, ON OR BEFORE'
036600980903     O                                           58 ' 30TH APRIL, '
036700011008     O                       new_year            62
036800980903     O                                           63 '.'
036900021101     O          e            regfin         1
037000980903     O                                           25 'ATTACH THE ORIGINAL OF '
037100980903     O                                           45 'THIS NOTICE TO YOUR '
037200980903     O                                           55 'INCOME TAX'
037300021101     O          e            regfin         2
037400980903     O                                           24 'RETURN AND RETAIN THE '
037500980903     O                                           43 'DUPLICATE FOR YOUR '
037600980903     O                                           51 'RECORDS.'
037700021101     O          e            regfin         1
037800980903     O                                           26 'IF YOU ARE NOT LIABLE TO'
037900980903     O                                           48 ' INCOME TAX YOU SHOULD'
038000021101     O          e            regfin         1
038100980903     O                                           26 'CLAIM REPAYMENT FROM THE'
038200980903     O                                           50 ' COMMISSIONER OF INLAND '
038300980903     O                                           58 'REVENUE '
038400021101     O          e            regfin         1
038500980903     O                                           25 'SUBMITTING AT THE SAME '
038600980903     O                                           46 'TIME THE ORIGINAL OF '
038700980903     O                                           58 'THIS NOTICE.'
038800021101     O          e            regfin         2
038900980903     O                                           23 'YOURS CO-OPERATIVELY,'
039000021101     O          e            regfin         2
039100980903     O                                           22 '....................'
039200021101     O          e            regfin         1
039300980903     O                                           36 'EMPLOYEE''S'
039400021101     O          e            regfin         1
039500980903     O                                           12 '|'
039600980903     O                                           36 '------------------------'
039700980903     O                                           50 '--------------'
039800980903     O                                           51 '|'
039900021101     O          e            regfin         1
040000980903     O                                           12 '|'
040100080409     O                                           23 'TAX ID.   '
040200980903     O                                           25 '|'
040300980903     O                                           36 '|'
040400980903     O                                           43 'SOCIAL'
040500980903     O                                           51 '|'
040600021101     O          e            regfin         1
040700980903     O                                           12 '|'
040800080409     O                                           21 'NUMBER  '
040900980903     O                                           25 '|'
041000980903     O                                           36 '|'
041100980903     O                                           49 'SECURITY NO.'
041200980903     O                                           51 '|'
041300021101     O          e            regfin         1
041400980903     O                                           12 '|'
041500980903     O                                           25 '|'
041600980903     O                                           36 '|'
041700980903     O                                           36 '------------------------'
041800980903     O                                           50 '--------------'
041900980903     O                                           51 '|'
042000021101     O          e            regfin         1
042100980903     O                                           12 '|'
042200980903     O                                           25 '|'
042300980903     O                                           36 '|'
042400011008     O                       irdnum              23
042500011008     O                       nisnum              49
042600980903     O                                           51 '|'
042700021101     O          e            regfin         2
042800980903     O                                           12 '|'
042900980903     O                                           25 '|'
043000980903     O                                           36 '------------------------'
043100980903     O                                           50 '--------------'
043200980903     O                                           51 '|'
043300021101     O          e            regfin         1
043400980903     O                                            7 'NAME:'
043500011008     O                       name           B    37
043600021101     O          e            regfin         1
043700980903     O                                           10 'ADDRESS:'
043800020326     O                                           16 'C/O'
043900020326     O                       coname            +  1
044000021101     O          e            regfin         1
044100010116     O                                           12 'I.R.D. 7B2'
044200050422
044300050422     P*-----------------------------------------------
044400050422     P* Procedure name: getStrDate
044500050422     P* Purpose:  Get start date of employee earnings
044600050422     P* Returns:        1 (start date)
044700050422     P* Parameter:      none
044800050422     P*-----------------------------------------------
044900050422     P getStrDate      B
045000050422     D getStrDate      PI              d
045100050422
045200050422     D strdate         s              8  0
045300050422
045400050422      /free
045500050422
045600050422            strdate = %int(%char(pay_year) + '0101');
045700050422
045800050422            open(e) payhisl05;
045900050422            setll (emp:strdate) phisfmt;
046000050422            reade emp phisfmt;
046100050422            if %eof;
046200050422              close(e) payhisl05;
046300050422              return %date(emply:*eur);
046400050422             else;
046500050422              close(e) payhisl05;
046600050422              if %date(emply:*eur) <
046700050422                  %date(phisndate:*iso);
046800050422                return %date(emply:*eur);
046900050422               else;
047000050422                return %date(phisndate:*iso);
047100050422              endif;
047200050422            endif;
047300050422
047400050422
047500050422      /end-free
047600050422     P getStrDate      E
047700180118
047800180118       // --------------------------------------------------
047900180118       // Procedure name: getNISValuePrc
048000180118       // Purpose:
048100180118       // Returns:
048200180118       // Parameter:      emp_in
048300180118       // Parameter:      income_year
048400180118       // Parameter:      NIS_Code
048500180118       // --------------------------------------------------
048600180118       DCL-PROC getNISValuePrc ;
048700180118         DCL-PI *N PACKED(11:2);
048800180118           emp_in PACKED(6:0);
048900180118           income_year PACKED(4:0);
049000180118           NIS_Code PACKED(2:0);
049100180118         END-PI ;
049200180118         // Your local fields go here
049300180118
049400180118         DCL-S retField PACKED(11:2) inz(0);
049500180118         // Your calculation code goes here
049600180118           exec sql
049700180118           select sum(ytdamt) into :retField from PAYEYTD
049800180118             where emp = :emp_in and year = :income_year
049900180118             and ytdtype = :NIS_Code;
050000180118
050100180118
050200180118         return retField ;
050300180118       END-PROC ;
050400180118**
050500011012       Year-end Income Tax Letter

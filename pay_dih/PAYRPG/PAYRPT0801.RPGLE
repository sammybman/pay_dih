000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200950726     F** LISTING PAYROLL YEAR TO DATES
000300011003     F** ADJUSTMENTS BY------T.I.BYNOE  *** R.WILLIAMS
000400011003     F**                     C.WILLIAMS   & S.BRITTON
000500021030     Fpayeytdl03if   e           k disk
000600011003     Fqsysprt   O    f  132        printer oflind(*inof)
000700011003     ** Payroll PROTOTYPES
000800151021      /copy qcpysrc,genPR
000900151021      /copy qcpysrc,empPR
001000021030     **
001100021030     D  ytd_list       ds                  import
001200021030     D  pcdftytdDS                   10
001300021030     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
001400021030     **
001500021030     D hdr_text        s              9    dim(99)
001600021030     D hdr_type        s              2  0 dim(99)
001700021030     D det_amt         s             11  2 dim(99)
001800021031     D tot_amt         s             13  2 dim(99)
001900020118     D pay_year        s              4  0
002000021030     D last_emp        s                   like(emp)
002100021030     D last_dept       s                   like(dept)
002200021030     D last_name       s                   like(name)
002300020118     D seq             s              1  0
002400021029     D sel             s              1  0
002500021030     D xcnt            s              3  0 inz(1)
002600021030     D counter         s              3  0 inz(1)
002700050525     D countr          s              5  0
002800021031     D cnt             s              3  0 inz(1)
002900021031     D cnt2            s              3  0 inz(2)
003000021031     D cnt3            s              3  0 inz(3)
003100021031     D cnt4            s              3  0 inz(4)
003200021031     D cnt5            s              3  0 inz(5)
003300021030     D ycnt            s              3  0
003400020122     D coltxt          s             37
003500020118     D name            s             30
003600020118     D coname          s             30
003700020118     D @title          s             40
003800020118     D @date           s             10
003900020118     D arr             S             40    dim(1) ctdata perrcd(1)
004000020118     **
004100021030     D limit           c                   5
004200021030     D blank_text      c                   'GROSS '
004300020118     D numcnst         c                   '   EMPLOYEE NO.  &  DEPARTMENT  '
004400020122     D namcnst         c                   'EMPLOYEE NAME        &         NUMB-
004500020122     D                                     ER'
004600011003     **
004700950930     D                SDS
004800950930     D  @PGM             *PROC
004900950930     D  @USER                254    263
005000011003     **
005100020118
005200021030      /free
005300021030          exsr  aainit;
005400021030          select;
005500021030           when  seq = 1;
005600021030             coltxt = numcnst;
005700021030           when  seq = 2;
005800021030             coltxt = namcnst;
005900021030          endsl;
006000021030
006100021031         exsr write_heading;
006200021031
006300021030
006400021030         setll  pay_year  ytdrec;
006500021030         reade  pay_year  ytdrec;
006600021030
006700021030          exsr get_name_text;
006800021030          last_emp = emp;
006900021030          last_dept = dept;
007000021030          last_name = name;
007100021030
007200021030          dow not(%eof);
007300021030
007400021030             ycnt = %lookup(ytdtype:hdr_type);
007500021030
007600021030              // employee number changed
007700021030                if emp <> last_emp;
007800021030                  exsr print_details;
007900021030                  last_emp = emp;
008000021030                  last_dept = dept;
008100021030                  exsr get_name_text;
008200021031                  last_name = name;
008300021030                  clear det_amt;
008400021030                endif;
008500021031
008600021030                if ycnt <> 0;
008700021030                  det_amt(ycnt) = det_amt(ycnt) + ytdamt;
008800021030                  tot_amt(ycnt) = tot_amt(ycnt) + ytdamt;
008900021030                endif;
009000021030            reade  pay_year  ytdrec;
009100021030          enddo;
009200021030
009300021030          exsr print_details;
009400021031          exsr write_final;
009500021030          eval *inlr = *on;
009600021030          return;
009700021030
009800021030       //----------
009900021030       // Get Name
010000021030       //----------
010100021030       begsr get_name_text;
010200021030             name = surnam;
010300021030             if   christ <> *blanks;
010400021030                      name = %trim(name) + ' ,' +
010500021030                       %triml(christ);
010600021030             endif;
010700021030       endsr;
010800021031
010900021031       //---------------
011000021031       // Write Heading
011100021031       //---------------
011200021031       begsr write_heading;
011300021031         exsr reset_counters;
011400021031         except  hedr;
011500021031         // Handle Multiple line heading
011600021031         dow cnt5 < xcnt;
011700021031         exsr increment_counters;
011800021031         except  hedrincr;
011900021031         enddo;
012000021031
012100021031         except  hedrnext;
012200021031       endsr;
012300021031
012400021031       //---------------
012500021031       // Write Detail
012600021031       //---------------
012700021031       begsr write_detail;
012800021031         exsr reset_counters;
012900021219          if (det_amt(cnt) + det_amt(cnt2) +
013000021219             det_amt(cnt3) + det_amt(cnt4) +
013100021219             det_amt(cnt5)) <> *zeros;
013200021219            except  detail;
013300021219          endif;
013400021031         // Handle Multiple line details
013500021031         dow cnt5 < xcnt;
013600021220          exsr increment_counters;
013700021220          if (det_amt(cnt) + det_amt(cnt2) +
013800021220             det_amt(cnt3) + det_amt(cnt4) +
013900021220             det_amt(cnt5)) <> *zeros;
014000021220           except  detail;
014100021220          endif;
014200021031         enddo;
014300021031
014400021031       endsr;
014500021031
014600021031       //-------------------
014700021031       // Write Final totals
014800021031       //-------------------
014900021031       begsr write_final;
015000021031         exsr reset_counters;
015100021031         except  final;
015200021031         // Handle Multiple line details
015300021031         dow cnt5 < xcnt;
015400021031         exsr increment_counters;
015500021031         except  final;
015600021031         enddo;
015700021031
015800021031       endsr;
015900021031
016000021031       //--------------------
016100021031       // Increment Counters
016200021031       //--------------------
016300021031       begsr increment_counters;
016400021031            cnt = cnt + 5;
016500021031            cnt2= cnt2 + 5;
016600021031            cnt3= cnt3 + 5;
016700021031            cnt4= cnt4 + 5;
016800021031            cnt5= cnt5 + 5;
016900021031       endsr;
017000021031       //----------------
017100021031       // Reset Counters
017200021031       //----------------
017300021031       begsr reset_counters;
017400021031         reset cnt;
017500021031         reset cnt2;
017600021031         reset cnt3;
017700021031         reset cnt4;
017800021031         reset cnt5;
017900021031       endsr;
018000021031
018100021030       //-----------------------
018200021030       // Print Employee datails
018300021030       //-----------------------
018400021030       begsr print_details;
018500021030                   if  *inof;
018600021031                       exsr write_heading;
018700021030                   endif;
018800011003
018900030113          if %xfoot(det_amt) <> *zeros;
019000030113       // if (det_amt(cnt) + det_amt(cnt2) +
019100030113       //    det_amt(cnt3) + det_amt(cnt4) +
019200030113       //    det_amt(cnt5)) <> *zeros;
019300021030                   select;
019400021030                      when  seq = 1;
019500021030                        except  detailnum;
019600021030                      when  seq = 2;
019700021030                         except detailnam;
019800021030                   endsl;
019900030113          endif;
020000021031                   exsr write_detail;
020100021030
020200021030        endsr;
020300021030      /end-free
020400021031
020500011003     ***************************************************
020600011003     ** First initilisation routine for all programs  **
020700011003     ***************************************************
020800011003     C     aainit        Begsr
020900011003     C                   eval      coname = get_coname
021000011003     C                   eval      @date = get_sys_date
021100020118     C                   eval      @title = arr(1)
021200011003     **
021300011003     C                   Endsr
021400021030     *************************
021500021030     ** Start-up subroutine **
021600021030     *************************
021700021030     C     *inzsr        begsr
021800021030     C     *entry        plist
021900021030     C                   parm                    seq
022000021030     C                   parm                    pay_year
022100021030     C                   parm                    sel
022200021030     C
022300021030      /free
022400021030         select;
022500021030           when  sel = 1 or sel = 3;
022600021030              for counter to limit;
022700021030             ycnt = %lookup(dft_list(counter):hdr_type);
022800021030
022900021030               if ycnt = 0 or ycnt = xcnt;
023000021030                   hdr_type(xcnt) = dft_list(counter);
023100021030                     if dft_list(counter) = 0;
023200021030                       evalr  hdr_text(xcnt) = blank_text;
023300021030                         else;
023400050525                        get_payty_info('RD':dft_list(counter):payty_struc);
023500021031                        evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
023600021030                     endif;
023700021030                   xcnt = xcnt + 1;
023800021030               endif;
023900021030              endfor;
024000021030
024100021030           when  sel = 2;
024200050525            countr = *zeros;
024300050525            dow not get_payty_info('RD':-1:payty_struc:countr);
024400021031              evalr  hdr_text(xcnt) = %trim(ptyshrtnm);
024500021030               hdr_type(xcnt) = ptyptype;
024600021030              xcnt = xcnt + 1;
024700021030            enddo;
024800021030
024900021030         endsl;
025000021030      /end-free
025100021030     C                   endsr
025200020118     **
025300021031     Oqsysprt   E            hedr           2 04
025400021031     O                       @pgm                10
025500011004     O                       coname              72
025600011004     O                       @date              130
025700021031     O          E            hedr           2
025800021031     O                       @user               10
025900011004     O                       @title              80
026000011004     O                                          125 'Page:'
026100011004     O                       page          z    130
026200021031     O          E            hedr           2
026300011003     O                                           30 'EMPLOYEES'
026400950726     O                                           49 'CURRENT     Y.T.D.'
026500020118     O                                           71 'FIGURES  FOR  YEAR'
026600020118     O                       pay_year            76
026700021030
026800021031     O          E            hedr           1
026900020122     O                       coltxt              37
027000021031     O                       hdr_text(cnt)       53
027100021031     O                       hdr_text(cnt2)      71
027200021031     O                       hdr_text(cnt3)      89
027300021031     O                       hdr_text(cnt4)     107
027400021031     O                       hdr_text(cnt5)     125
027500021031     O          E            hedrincr       1
027600021031     O                       hdr_text(cnt)       53
027700021031     O                       hdr_text(cnt2)      71
027800021031     O                       hdr_text(cnt3)      89
027900021031     O                       hdr_text(cnt4)     107
028000021031     O                       hdr_text(cnt5)     125
028100021031     O          E            hedrnext       2
028200021030
028300020118     O          e            detailnum      0
028400021031     O                       last_emp      z     10
028500021031     O                       last_dept     z     25
028600020118     O          e            detailnam      0
028700021031     O                       last_name           30
028800021031     O                       last_emp      z     37
028900020118
029000021031     O          e            detail         1
029100020118     **
029200021031     O                       det_amt(cnt)  2     53
029300021031     O                       det_amt(cnt2) 2     71
029400021031     O                       det_amt(cnt3) 2     89
029500021031     O                       det_amt(cnt4) 2    107
029600021031     O                       det_amt(cnt5) 2    125
029700021030
029800021030     O          e            final       1
029900021030     **
030000021031     O                       tot_amt(cnt)  2     53
030100021031     O                       tot_amt(cnt2) 2   +  1
030200021031     O                       tot_amt(cnt3) 2   +  1
030300021031     O                       tot_amt(cnt4) 2   +  1
030400021031     O                       tot_amt(cnt5) 2   +  1
030500011003**
030600011003 Payroll Earnings Year-to-Date Report

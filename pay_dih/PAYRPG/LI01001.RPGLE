000100950605     H DECEDIT('.') DATEDIT(*DMY/)
000200980409     **
000300980409     ** EMPLOYEES CREDIT STATUS REPORT
000400980409     ** PROGRAMMER------Samuel Britton
000500021003     F* Employee Master File
000600021004     Fempmast   if   e           k disk
000700021003     F* Liability Master File
000800021003     Fliabml02  if   e           k disk
000900021003     Fqsysprt   o    f  132        printer oflind(*inof)
001000021003     **
001100151009      /copy *libl/qcpysrc,PAYPR
001200151009      /copy qcpysrc,genpr
001300151009      /copy qcpysrc,emppr
001400151009
001500151009       Dcl-DS  SeveranceData   likeDS(SeveranceInfoDS);
001600151009
001700021003     **
001800021003     Dlia_check        ds                  occurs(80) inz
001900021003     D lia_type                       2  0
002000021003     D lia_desc                      30
002100021003     D lia_total                     12  2
002200980409     **
002300021004     D typarr          s              2  0 dim(99)                              TOTAL AMOUNT
002400021004     D liaarr          s              2  0 dim(99)                              TOTAL AMOUNT
002500980409     **
002600980409     D la              s             11  2 dim(12)
002700021003     D xcnt            s              3  0 inz(0)                               TOTAL AMOUNT
002800021004     D counter         s              3  0 inz(1)                               TOTAL AMOUNT
002900050513     D countr          s              5  0                                      TOTAL AMOUNT
003000030521     D*asset_flag      s              1                                         TOTAL AMOUNT
003100021003     D asset_dsp       s             30                                         TOTAL AMOUNT
003200021003     D coname          s             30                                         TOTAL AMOUNT
003300021003     D name            s             40                                         TOTAL AMOUNT
003400021003     D type_hld        s                   like(litype)                         TOTAL AMOUNT
003500021003     D total_liab      s             11  2                                      TOTAL AMOUNT
003600021003     D total_asset     s             11  2                                      TOTAL AMOUNT
003700021003     D employee_liab   s             11  2                                      TOTAL AMOUNT
003800021003     D employee_asset  s             11  2                                      TOTAL AMOUNT
003900021003     D employee_var    s             11  2                                      TOTAL AMOUNT
004000021003     D month           s              9
004100021003     D FirstRecord     s               n
004200030521     **
004300030521     D                 ds
004400030521     D prdds                          6  0
004500030521     D year                           4  0 overlay(prdds:1)
004600030521     D mth                            2  0 overlay(prdds:*next)                 TOTAL AMOUNT
004700980409     **
004800970305     D                SDS
004900970305     D  @PGM             *PROC
005000970305     D  @USER                254    263
005100970305     D  @SDATE               276    281  0
005200030520
005300021003     ** SubProcedure PROTOTYPE
005400030520     DLiabEntry        PR                  extproc('LI01001')
005500030521     D                                6  0
005600030521     D                                1
005700030521
005800030521     DLiabEntry        PI
005900030521     D prno                           6  0
006000030521     D asset_flag                     1
006100021003     **
006200151009     DSAVINGS          PR                  extpgm('EMPPRF02')
006300021003     D emp                            6  0
006400021003     D trn_value                     11  2
006500021003     D period                         6  0
006600980409     **
006700980409     Ilifmt
006800980409     I              liamt01                     la(01)
006900980409     I              liamt02                     la(02)
007000980409     I              liamt03                     la(03)
007100980409     I              liamt04                     la(04)
007200980409     I              liamt05                     la(05)
007300980409     I              liamt06                     la(06)
007400980409     I              liamt07                     la(07)
007500980409     I              liamt08                     la(08)
007600980409     I              liamt09                     la(09)
007700980409     I              liamt10                     la(10)
007800980409     I              liamt11                     la(11)
007900980409     I              liamt12                     la(12)
008000021003
008100021003      /free
008200021003
008300030521          prdds = prno;
008400030521          month = get_month(mth);
008500030521          except report_hdr;
008600030521
008700021004        // setup liability types in array
008800021004        reset counter;
008900050513
009000050513        countr = *zeros;
009100050513        dow not get_ltype_info('RD':-1:ematf_struc:countr);
009200021004            if %subst(%char(emdept):1:1) = '1';
009300021004                if %lookup(emseltyp:liaarr) = 0;
009400021004                  liaarr(counter) = emseltyp;
009500050513                  counter += 1;
009600021004                endif;
009700021004            endif;
009800050513
009900021004        enddo;
010000021004
010100021004        setll *loval emprec;
010200021004        read emprec;
010300021004        dow not(%eof(empmast));
010400021003           name = %trim(christ) + ' ' + %triml(surnam);
010500030521           exsr print_header;
010600021003           exsr generate_employee_liab;
010700021003
010800021003           if employee_liab <> *zeros;
010900021003               except emp_dedtot;
011000021003           endif;
011100021003
011200021003             if asset_flag = 'Y';
011300021007                 if  FirstRecord;
011400021007                    except  emp_hdr;
011500021007                 endif;
011600021008                total_asset = *zeros;
011700021003                SAVINGS(emp:total_asset:prno);
011800021003                asset_dsp = 'Savings';
011900021003                employee_asset = total_asset;
012000021003                except emp_assets;
012100021003
012200151009             // total_asset  = SEV_CALC(status:get_basic_wage(emp):
012300151009             //          %date():%date(emply:*eur):resign);
012400151009                total_asset = sev_calc(emp:%date():%date(emply:*eur):
012500151009                                 resign:SeveranceData);
012600021003                asset_dsp = 'Severance';
012700021003                except emp_assets;
012800021003                employee_asset = employee_asset + total_asset;
012900030521
013000021003             // employee asset totals
013100021003                except emp_asttot;
013200030521
013300021003             // employee asset/liability variance
013400021003                employee_var = employee_asset - employee_liab;
013500021003                except emp_var;
013600021003
013700021003             endif;
013800021004        read emprec;
013900021004        enddo;
014000021003
014100021004          // Last Record summary
014200021004               *inlr = *on;
014300021004                   except rpt_summ_h;
014400021004                   reset counter;
014500021004                 for counter to xcnt;
014600021004                   %occur(lia_check) = counter;
014700021004                   counter = counter +1;
014800021004                   except rpt_summ;
014900021004                 endfor;
015000021003
015100021003          //-----------------------------
015200021003          // Generate employee liability
015300021003          //-----------------------------
015400021003          begsr generate_employee_liab;
015500021003             type_hld = litype;
015600021003             employee_liab = *zeros;
015700021003             FirstRecord = *on;
015800021004
015900021004             setll emp lifmt;
016000021004             reade emp lifmt;
016100021004               dow  not(%eof(liabml02));
016200021004                  if %lookup(litype:liaarr) > 0;
016300021004                    if type_hld <> litype;
016400021004                       exsr print_employee_detail;
016500021004                       type_hld = litype;
016600021004                    endif;
016700021004
016800021004                    total_liab = total_liab + la(mth);
016900021004                    employee_liab = employee_liab + la(mth);
017000021004                  endif;
017100021003
017200021003             reade emp lifmt;
017300021003               enddo;
017400021004             exsr print_employee_detail;
017500021003
017600021003          endsr;
017700021003
017800021003          //-------------------
017900021003          // Accumulate totals
018000021003          //-------------------
018100021003          begsr accum_totals;
018200021003
018300021003
018400021003        //Search for emp. type in array .if found set occurance level and
018500021003        //add total ,if not found add new array record and Data Struct. tot
018600021003
018700021004              if   %lookup(type_hld:typarr) = 0;
018800021003                   xcnt = xcnt + 1;
018900021003                   typarr(xcnt) = type_hld;
019000021003                   %occur(lia_check) = xcnt;
019100021003                   lia_type = type_hld;
019200021003                   lia_total = lia_total + total_liab;
019300021003                   lia_desc =  emdsp;
019400021003
019500021003                  else;
019600021003
019700021003                   %occur(lia_check) =  %lookup(type_hld:typarr);
019800021003                   lia_total = lia_total + total_liab;
019900021003              endif;
020000021003        endsr;
020100021003
020200021004          //----------------------
020300021004          // Print Employee Detail
020400021004          //----------------------
020500021004             begsr print_employee_detail;
020600021004                       if  total_liab <> *zeros;
020700021004                          if  FirstRecord;
020800021004                            except  emp_hdr;
020900021004                            FirstRecord = *off;
021000021004                          endif;
021100050513                         get_ltype_info('RD':type_hld:ematf_struc);
021200021004                         except  emp_total;
021300021004                         exsr accum_totals;
021400021004                         total_liab = *zeros;
021500021004                       endif;
021600021004             endsr;
021700021003
021800030521           begsr print_header;
021900030521            if *inof;
022000030521             except report_hdr;
022100030521            endif;
022200030521           endsr;
022300030521
022400030520           begsr *inzsr;
022500030521              coname = get_coname;
022600030521          //  month = get_month(mth);
022700030520            endsr;
022800021003      /end-free
022900021003
023000980409     **
023100030521     Oqsysprt   e            report_hdr     2 02
023200030521     O
023300021003     O                       @pgm                10
023400021003     O                       coname              77
023500021003     O                       @sdate        Y    130
023600030521     O          e            report_hdr     2
023700030521     O
023800021003     O                       @user               10
023900950605     O                                           68 'EMPLOYEE CREDIT STATUS A'
024000950605     O                                           72 'S AT'
024100021003     O                       month               82
024200021003     O                       year                87
024300030521     O          e            report_hdr     2
024400030521     O
024500021003     O                                            6 'EMP'
024600021003     O                                           15 'STATUS'
024700021003     O                                           22 'GRADE'
024800021003     O                                           28 'NAME'
024900950605     O                                          130 'TOTAL'
025000980409     **
025100021003     Oqsysprt   e            emp_hdr     1  1
025200980409     O                       emp           Z      6
025300021003     O                       status              15
025400021003     O                       class               22
025500021003     O                       name                64
025600021003
025700021003     Oqsysprt   e            emp_total   1
025800021003     O                       emdsp               60
025900021004     O                       total_liab    j   +  2
026000021003
026100021003     Oqsysprt   e            emp_dedtot
026200021003     O                                           95 'LIABILITY TOTAL'
026300021004     O                       employee_liab j   +  2
026400021003
026500021003     ** ASSETS **
026600021003     Oqsysprt   e            emp_assets  1
026700021003     O                       asset_dsp           60
026800021003     O                       total_asset   1   +  2
026900021003
027000021003     Oqsysprt   e            emp_asttot
027100021003     O                                           95 'ASSETS TOTAL...'
027200021003     O                       employee_asset1   +  2
027300021003
027400021003     Oqsysprt   e            emp_var     1  1
027500021003     O                                           95 'VARIANCE.......'
027600021003     O                       employee_var  A   +  2
027700021004     Oqsysprt   e            rpt_summ_h  2
027800021004     O                                           20 'FINAL TOTALS...'
027900021004     Oqsysprt   e            rpt_summ    1
028000021004     O                       lia_type             6
028100021004     O                       lia_desc          +  2
028200021004     O                       lia_total     A   +  2

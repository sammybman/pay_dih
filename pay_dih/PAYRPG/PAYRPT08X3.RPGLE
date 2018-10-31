000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200950726     F** LISTING PAYROLL YEAR TO DATES
000300011003     F**                     C.WILLIAMS   & S.BRITTON
000400021030     Fpayeytdl03if   e           k disk
000500011003     ** Payroll PROTOTYPES
000600011003      /copy genPR
000700050525      /copy empPR
000800090224      /copy *libl/payrpg,PAYPR
000900090225     D
001000090225     D*-------------------------------------
001100090225     D* Prototype for procedure: getEmpData
001200090225     D*-------------------------------------
001300090305     D getEmpData      PR             4  0
001400090225     D
001500090304
001600090304     D*-----------------------------------------
001700090304     D* Prototype for procedure: prepare_batch
001800090304     D*-----------------------------------------
001900090304     D prepare_batch   PR
002000090304
002100090304     D*-----------------------------------------
002200090304     D* Prototype for procedure: write_to_file
002300090304     D*-----------------------------------------
002400090304     D write_to_file   PR
002500090304     D option                         1    const
002600090304        // '1' - create/open file
002700090304        // '2' - write data
002800090304        // '3' - close file
002900090304     D data                         100    const varying options(*nopass)
003000090304
003100090304     D write_batch_detail...
003200090304     D                 PR
003300090304
003400021030     D  ytd_list       ds                  import
003500021030     D  pcdftytdDS                   10
003600021030     D dft_list                       2  0 dim(5) overlay(pcdftytdDS)
003700021030     **
003800090304
003900090305     DGra_struc        ds                  qualified  inz
004000090305     D counter                        6  0
004100090305     D transhdr                            likeDS(text_hdr)
004200090305     D transdet                            likeDS(text_det) dim(3000)
004300090305     D transftr                            likeDS(text_ftr)
004400090304
004500090304
004600090305     Dtext_hdr         ds                  template
004700090305     D head_type                      1    inz('H')
004800090305     D head_tin                      12    varying inz('|')
004900090305     D head_year                      5    varying inz('|')
005000090305     D head_date                     11    varying inz('|')
005100090305     D head_sal#                      6    varying inz('|')
005200090305     D head_salV                     11    varying inz('|')
005300090305     D head_oth#                      6    varying inz('|')
005400090305     D head_othV                     11    varying inz('|')
005500090305     D head_sal2#                     6    varying inz('|')
005600090305     D head_sal2V                    11    varying inz('|')
005700090305     D head_oth2#                     6    varying inz('|')
005800090305     D head_oth2V                    11    varying inz('|')
005900090304
006000090305     Dtext_det         ds                  template
006100090305     D det_type                       1    inz('D')
006200090305     D det_tin                       12    varying  inz('|')
006300090305     D det_months                     2    varying  inz('|')
006400090305     D det_emptyp                     1    varying  inz('|')
006500090305     D det_pretin                    12    varying  inz('|')
006600090305     D det_nxttin                    12    varying  inz('|')
006700090305     D det_tot7a                     12    varying  inz('|')
006800090305     D det_tot7b                     12    varying  inz('|')
006900090305     D det_tot7c                     12    varying  inz('|')
007000090305     D det_total                     12    varying  inz('|')
007100090305     D det_paye                      12    varying  inz('|')
007200090304
007300090305     D text_ftr        ds                  template
007400090305     D foot_type                      1    inz('T')
007500090305     D foot_tot7a                    13    varying inz('|')
007600090305     D foot_tot7b                    13    varying inz('|')
007700090305     D foot_tot7c                    13    varying inz('|')
007800090305     D foot_total                    13    varying inz('|')
007900090305     D foot_paye                     13    varying inz('|')
008000090304
008100090305       // test footer
008200090305     D gra_ftr         ds                  qualified
008300090305
008400090304     D DIGITS          c                   '0123456789'
008500090304     D CRLF            c                   x'0D25'
008600090304
008700021030     D hdr_type        s              2  0 dim(99)
008800090304     D det_amt         s             11  0 dim(99)
008900090304     D tot_amt         s             13  0 dim(99)
009000020118     D pay_year        s              4  0
009100021030     D last_emp        s                   like(emp)
009200090305     D last_ird        s                   like(irdnum)
009300090305     D last_emply      s                   like(emply)
009400090305     D last_resign     s                   like(resign)
009500020118     D seq             s              1  0
009600021029     D sel             s              1  0
009700090304     D counter         s              3  0 inz(1)
009800021030     D xcnt            s              3  0 inz(1)
009900021031     D cnt             s              3  0 inz(1)
010000021031     D cnt2            s              3  0 inz(2)
010100021031     D cnt3            s              3  0 inz(3)
010200021031     D cnt4            s              3  0 inz(4)
010300021031     D cnt5            s              3  0 inz(5)
010400021030     D ycnt            s              3  0
010500020118     D coname          s             30
010600020118     D @date           s             10
010700090304     D filename        s             25    inz('/diserviq/payroll/gra.txt')
010800090220     D noMonths        s              4  0
010900090224     D partEmp         s              1
011000090305     D empCnt          s              6  0
011100090305     D empOthcnt       s              6  0
011200090305     D empsal          s             11  0
011300090305     D empothsal       s             11  0
011400090224     D int_yrend       s              8  0
011500090224     D int_yrstr       s              8  0
011600090305
011700090305     D foot_type       s              1    inz('T')
011800090305     D foot_tot7a      s             13    varying inz('|')
011900090305     D foot_tot7b      s             13    varying inz('|')
012000090305     D foot_tot7c      s             13    varying inz('|')
012100090305     D foot_total      s             13    varying inz('|')
012200090305     D foot_paye       s             13    varying inz('|')
012300020118     **
012400090304     D PIPE            c                   '|'
012500090224     D YREND_DATE      c                   '3112'
012600090224     D YRSTR_DATE      c                   '0101'
012700090220     D LIMIT           c                   5
012800090220     D MONTHSINYEAR    c                   12
012900011003     **
013000950930     D                SDS
013100950930     D  @PGM             *PROC
013200950930     D  @USER                254    263
013300011003     **
013400020118
013500021030      /free
013600090304         coname = get_coname;
013700021030
013800021030         setll  pay_year  ytdrec;
013900021030         reade  pay_year  ytdrec;
014000021030
014100021030          last_emp = emp;
014200090220          last_ird = irdnum;
014300090305          last_emply = emply;
014400090305          last_resign = resign;
014500090224
014600021030
014700021030          dow not(%eof);
014800021030
014900021030             ycnt = %lookup(ytdtype:hdr_type);
015000021030
015100021030              // employee number changed
015200021030                if emp <> last_emp;
015300090305                   write_batch_detail();
015400090305
015500090305                   last_emp = emp;
015600090305                   last_ird = irdnum;
015700090305                   last_emply = emply;
015800090305                   last_resign = resign;
015900090224
016000090224
016100021030                  clear det_amt;
016200021030                endif;
016300021031
016400021030                if ycnt <> 0;
016500090304                  det_amt(ycnt) += ytdamt;
016600090304                  tot_amt(ycnt) += ytdamt;
016700021030                endif;
016800090220
016900021030            reade  pay_year  ytdrec;
017000021030          enddo;
017100021030
017200090304
017300090305          write_batch_detail();
017400090305          prepare_batch();
017500090305
017600021030          eval *inlr = *on;
017700021030          return;
017800021031
017900021030      /end-free
018000021031
018100021030     *************************
018200021030     ** Start-up subroutine **
018300021030     *************************
018400021030     C     *inzsr        begsr
018500021030     C     *entry        plist
018600021030     C                   parm                    seq
018700021030     C                   parm                    pay_year
018800021030     C                   parm                    sel
018900021030     C
019000021030      /free
019100090304         select;
019200090304           when  sel = 1 or sel = 3;
019300090304              for counter to limit;
019400090304             ycnt = %lookup(dft_list(counter):hdr_type);
019500090304
019600090304               if ycnt = 0 or ycnt = xcnt;
019700090304                   hdr_type(xcnt) = dft_list(counter);
019800090304                   xcnt += 1;
019900090304               endif;
020000090304              endfor;
020100090304         endsl;
020200090304
020300021030      /end-free
020400021030     C                   endsr
020500020118     **
020600090225
020700090225     P*-----------------------------------------------
020800090225     P* Procedure name: getStrDate
020900090225     P* Purpose:  Get start date of employee earnings
021000090225     P* Returns:        1 (start date)
021100090225     P* Parameter:      none
021200090225     P*-----------------------------------------------
021300090225     P getEmpData      B
021400090305     D getEmpData      PI             4  0
021500090225
021600090304     D noMonths        s              4  0
021700090225
021800090225      /free
021900090225
022000090225          // no. of months employed
022100090225          int_yrend = %int(yrend_date + %char(pay_year));
022200090225          int_yrstr = %int(yrstr_date + %char(pay_year));
022300090225
022400090305          if last_resign = *zeros;
022500090305            if %date(int_yrstr:*eur) > %date(last_emply:*eur);
022600090225             noMonths = %diff(%date(int_yrend:*eur):
022700090225                              %date(int_yrstr:*eur) - %days(1):*months);
022800090225              else;
022900090225             noMonths = %diff(%date(int_yrend:*eur):
023000090305                              %date(last_emply:*eur)- %days(1):*months);
023100090225            endif;
023200090225
023300090225             else;
023400090225
023500090305            if %date(int_yrstr:*eur) > %date(last_emply:*eur);
023600090305             noMonths = %diff(%date(last_resign:*eur):
023700090225                              %date(int_yrstr:*eur) - %days(1):*months);
023800090225              else;
023900090305             noMonths = %diff(%date(last_resign:*eur):
024000090305                              %date(last_emply:*eur) - %days(1):*months);
024100090225            endif;
024200090225          endif;
024300090225
024400090225          if  noMonths >= MONTHSINYEAR;
024500090225             noMonths = MONTHSINYEAR;
024600090225             partEmp = *blank;
024700090305             empcnt += 1;
024800090305             empsal += det_amt(1);
024900090225            else;
025000090225             partEmp = 'P';
025100090305             empOthcnt += 1;
025200090305             empOthsal += det_amt(1);
025300090225          endif;
025400090304
025500090304          return noMonths;
025600090225
025700090225      /end-free
025800090225     P getEmpData      E
025900090304
026000090304
026100090304     P*-----------------------------------------
026200090304     P* Procedure name: prepare_batch
026300090304     P* Purpose:        prepare batch
026400090304     P* Returns:        0
026500090304     P* Parameter:      0
026600090304     P*-----------------------------------------
026700090304     P prepare_batch   B
026800090304     D prepare_batch   PI
026900090304
027000090305     D xcnt            s              6  0
027100090304
027200090304      /free
027300090304
027400090304         //  ...create/open file;
027500090304         //----------------------
027600090305           write_to_file('1');
027700090305
027800090305         //  write header record;
027900090305           gra_struc.transhdr.head_tin += %char(tot_amt(1));
028000090305           gra_struc.transhdr.head_tin += %char(tot_amt(1));
028100090305           gra_struc.transhdr.head_year += %char(pay_year);
028200090305           gra_struc.transhdr.head_date += get_sys_date();
028300090305           gra_struc.transhdr.head_sal# += %char(empcnt);
028400090305           gra_struc.transhdr.head_salv += %char(empsal);
028500090305           gra_struc.transhdr.head_oth# += %char(empothcnt);
028600090305           gra_struc.transhdr.head_othv += %char(empothsal);
028700090305           gra_struc.transhdr.head_sal2# += %char(empcnt);
028800090305           gra_struc.transhdr.head_sal2v += %char(empsal);
028900090305           gra_struc.transhdr.head_oth2# += %char(empothcnt);
029000090305           gra_struc.transhdr.head_oth2v += %char(empothsal);
029100090305
029200090305       ////write_to_file('2':gra_struc.transhdr);
029300090304
029400090304         //  write detail records;
029500090304         //------------------------
029600090304
029700090305           for xcnt = 1 to gra_struc.counter;
029800090305
029900090305              //write detail lines
030000090305         //// write_to_file('2':%trim(gra_struc.transdet(xcnt)));
030100090304
030200090305           endfor;
030300090304
030400090304
030500090305          // write footer record;
030600090304
030700090305           gra_ftr.foot_tot7a += %char(tot_amt(1));
030800090305           gra_ftr.foot_tot7b += '0';
030900090305           gra_ftr.foot_tot7c += '0';
031000090305           gra_ftr.foot_total += %char(tot_amt(1));
031100090305           gra_ftr.foot_paye  += %char(tot_amt(2));
031200090305           gra_struc.transftr.foot_tot7a += %char(tot_amt(1));
031300090305           gra_struc.transftr.foot_tot7b += '0';
031400090305           gra_struc.transftr.foot_tot7c += '0';
031500090305           gra_struc.transftr.foot_total += %char(tot_amt(1));
031600090305           gra_struc.transftr.foot_paye  += %char(tot_amt(2));
031700090305
031800090305           write_to_file('2':%trim(gra_ftr));
031900090305        // write_to_file('2':%trim(gra_struc.transftr));
032000090304
032100090305         //close file
032200090305           write_to_file('3');
032300090304
032400090304      /end-free
032500090304     P prepare_batch   E
032600090304
032700090304     P*-----------------------------------------
032800090304     P* Procedure name: write_to_file
032900090304     P* Purpose:        write to file
033000090304     P* Returns:        0
033100090304     P* Parameter:      0
033200090304     P*-----------------------------------------
033300090304     P write_to_file   B
033400090304     D write_to_file   PI
033500090304     D option                         1    const
033600090304     D data                         100    const varying options(*nopass)
033700090304
033800090304     D/copy IFSPR
033900090304     D fd              S             10I 0
034000090304     D wrdata          S            100A
034100090304     D rddata          S            100A
034200090304     D flags           S             10U 0
034300090304     D mode            S             10U 0
034400090304     D Msg             S             50A
034500090304     D Len             S             10I 0
034600090304      /free
034700090304
034800090304       //*************************************************************
034900090304       // writing data to a stream file
035000090304       //****************************************************************
035100090304         select;
035200090304           when option = '1';
035300090304
035400090304             fd = unlink(filename);
035500090304
035600090304             flags = O_WRONLY + O_CREAT + O_TRUNC +
035700090304                     O_TEXTDATA + O_CODEPAGE;
035800090304
035900090304             mode =  S_IRUSR + S_IWUSR
036000090304                         + S_IRGRP + S_IROTH;
036100090304
036200090304             // Open IFS file in ASCII format
036300090304             fd = open(filename: flags: mode: 819);
036400090304
036500090304             if  fd < 0;
036600090304             // Msg = 'open(): failed for writing';
036700090304             // return;
036800090304             endif;
036900090304
037000090304             // Close IFS file
037100090304             callp  close(fd);
037200090304
037300090304             // Reopen FS file
037400090304             fd = open(filename:O_WRONLY + O_TEXTDATA);
037500090304
037600090304           // Write some data
037700090304           when option = '2';
037800090304             wrdata = data + CRLF;
037900090304             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
038000090304
038100090304           // close the file
038200090304           when option = '3';
038300090304             callp  close(fd);
038400090304         endsl;
038500090304
038600090304      /end-free
038700090304     P write_to_file   E
038800090304
038900090304     P*-----------------------------------------
039000090304     P* Procedure name: write_batch_detail
039100090304     P* Purpose:        write batch detail lines
039200090304     P* Returns:        0
039300090304     P* Parameter:      0
039400090304     P*-----------------------------------------
039500090305     P write_batch_detail...
039600090304     P                 B
039700090304     D write_batch_detail...
039800090304     D                 PI
039900090304      /free
040000090305             //write detail lines
040100090305             if %xfoot(det_amt) <> *zeros;
040200090305
040300090305               gra_struc.counter += 1;
040400090305               if gra_struc.counter = 1;
040500090305                 dsply gra_struc.transdet(1).det_tin;
040600090305               endif;
040700090305               gra_struc.transdet(gra_struc.counter).
040800090305                     det_tin = PIPE + last_ird;
040900090305               if gra_struc.counter = 1;
041000090305                 dsply gra_struc.transdet(1).det_tin;
041100090305               endif;
041200090305               gra_struc.transdet(gra_struc.counter).
041300090305                     det_months += %char(getEmpData);
041400090305               gra_struc.transdet(gra_struc.counter).
041500090305                     det_emptyp += partemp;
041600090305               gra_struc.transdet(gra_struc.counter).
041700090305                     det_pretin += '0';
041800090305               gra_struc.transdet(gra_struc.counter).
041900090305                     det_nxttin += '0';
042000090305               gra_struc.transdet(gra_struc.counter).
042100090305                     det_tot7a += %char(det_amt(1));
042200090305               gra_struc.transdet(gra_struc.counter).
042300090305                     det_tot7b += '0';
042400090305               gra_struc.transdet(gra_struc.counter).
042500090305                     det_tot7c += '0';
042600090305               gra_struc.transdet(gra_struc.counter).
042700090305                     det_total += %char(det_amt(1));
042800090305               gra_struc.transdet(gra_struc.counter).
042900090305                     det_paye += %char(det_amt(2));
043000090305
043100090305             endif;
043200090304
043300090304      /end-free
043400090304     P write_batch_detail...
043500090304     P                 E
043600090304

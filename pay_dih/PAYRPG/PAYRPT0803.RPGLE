000100950726     H DECEDIT('.') DATEDIT(*DMY/)
000200950726     F** LISTING PAYROLL YEAR TO DATES
000300011003     F**                     C.WILLIAMS   & S.BRITTON
000400021030     Fpayeytdl03if   e           k disk
000500011003     ** Payroll PROTOTYPES
000600140307      /copy qcpysrc,genPR
000700140307      /copy qcpysrc,empPR
000800140307      /copy qcpysrc,PAYPR
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
004100090305     D line                         200    varying dim(3000)
004200090305
004300090304     D CRLF            c                   x'0D25'
004400090304
004500021030     D hdr_type        s              2  0 dim(99)
004600090304     D det_amt         s             11  0 dim(99)
004700090304     D tot_amt         s             13  0 dim(99)
004800020118     D pay_year        s              4  0
004900021030     D last_emp        s                   like(emp)
005000090305     D last_ird        s                   like(irdnum)
005100090305     D last_emply      s                   like(emply)
005200090305     D last_resign     s                   like(resign)
005300020118     D seq             s              1  0
005400021029     D sel             s              1  0
005500090304     D counter         s              3  0 inz(1)
005600021030     D xcnt            s              3  0 inz(1)
005700021031     D cnt             s              3  0 inz(1)
005800021031     D cnt2            s              3  0 inz(2)
005900021031     D cnt3            s              3  0 inz(3)
006000021031     D cnt4            s              3  0 inz(4)
006100021031     D cnt5            s              3  0 inz(5)
006200021030     D ycnt            s              3  0
006300020118     D coname          s             30
006400020118     D @date           s             10
006500090304     D filename        s             25    inz('/diserviq/payroll/gra.txt')
006600090220     D noMonths        s              4  0
006700090224     D partEmp         s              1
006800090305     D empCnt          s              6  0
006900090305     D empOthcnt       s              6  0
007000090305     D empsal          s             11  0
007100090305     D empothsal       s             11  0
007200090224     D int_yrend       s              8  0
007300090224     D int_yrstr       s              8  0
007400090305       // Line data
007500090305     D gra_line        s            200    varying inz
007600090305
007700090305     D field           s             20    varying dim(20)
007800090305
007900020118     **
008000090304     D PIPE            c                   '|'
008100090224     D YREND_DATE      c                   '3112'
008200090224     D YRSTR_DATE      c                   '0101'
008300090220     D LIMIT           c                   5
008400090220     D MONTHSINYEAR    c                   12
008500011003     **
008600950930     D                SDS
008700950930     D  @PGM             *PROC
008800950930     D  @USER                254    263
008900011003     **
009000020118
009100021030      /free
009200090304         coname = get_coname;
009300021030
009400021030         setll  pay_year  ytdrec;
009500021030         reade  pay_year  ytdrec;
009600021030
009700021030          last_emp = emp;
009800090220          last_ird = irdnum;
009900090305          last_emply = emply;
010000090305          last_resign = resign;
010100090224
010200021030
010300021030          dow not(%eof);
010400021030
010500021030             ycnt = %lookup(ytdtype:hdr_type);
010600021030
010700021030              // employee number changed
010800021030                if emp <> last_emp;
010900090305                   write_batch_detail();
011000090305
011100090305                   last_emp = emp;
011200090305                   last_ird = irdnum;
011300090305                   last_emply = emply;
011400090305                   last_resign = resign;
011500090224
011600090224
011700021030                  clear det_amt;
011800021030                endif;
011900021031
012000021030                if ycnt <> 0;
012100090304                  det_amt(ycnt) += ytdamt;
012200090304                  tot_amt(ycnt) += ytdamt;
012300021030                endif;
012400090220
012500021030            reade  pay_year  ytdrec;
012600021030          enddo;
012700021030
012800090304
012900090305          write_batch_detail();
013000090305          prepare_batch();
013100090305
013200021030          eval *inlr = *on;
013300021030          return;
013400021031
013500021030      /end-free
013600021031
013700021030     *************************
013800021030     ** Start-up subroutine **
013900021030     *************************
014000021030     C     *inzsr        begsr
014100021030     C     *entry        plist
014200021030     C                   parm                    seq
014300021030     C                   parm                    pay_year
014400021030     C                   parm                    sel
014500021030     C
014600021030      /free
014700090304         select;
014800090304           when  sel = 1 or sel = 3;
014900090304              for counter to limit;
015000090304             ycnt = %lookup(dft_list(counter):hdr_type);
015100090304
015200090304               if ycnt = 0 or ycnt = xcnt;
015300090304                   hdr_type(xcnt) = dft_list(counter);
015400090304                   xcnt += 1;
015500090304               endif;
015600090304              endfor;
015700090304         endsl;
015800090304
015900021030      /end-free
016000021030     C                   endsr
016100020118     **
016200090225
016300090225     P*-----------------------------------------------
016400090225     P* Procedure name: getStrDate
016500090225     P* Purpose:  Get start date of employee earnings
016600090225     P* Returns:        1 (start date)
016700090225     P* Parameter:      none
016800090225     P*-----------------------------------------------
016900090225     P getEmpData      B
017000090305     D getEmpData      PI             4  0
017100090225
017200090304     D noMonths        s              4  0
017300090225
017400090225      /free
017500090225
017600090225          // no. of months employed
017700090225          int_yrend = %int(yrend_date + %char(pay_year));
017800090225          int_yrstr = %int(yrstr_date + %char(pay_year));
017900090225
018000090305          if last_resign = *zeros;
018100090305            if %date(int_yrstr:*eur) > %date(last_emply:*eur);
018200090225             noMonths = %diff(%date(int_yrend:*eur):
018300090225                              %date(int_yrstr:*eur) - %days(1):*months);
018400090225              else;
018500090225             noMonths = %diff(%date(int_yrend:*eur):
018600090305                              %date(last_emply:*eur)- %days(1):*months);
018700090225            endif;
018800090225
018900090225             else;
019000090225
019100090305            if %date(int_yrstr:*eur) > %date(last_emply:*eur);
019200090305             noMonths = %diff(%date(last_resign:*eur):
019300090225                              %date(int_yrstr:*eur) - %days(1):*months);
019400090225              else;
019500090305             noMonths = %diff(%date(last_resign:*eur):
019600090305                              %date(last_emply:*eur) - %days(1):*months);
019700090225            endif;
019800090225          endif;
019900090225
020000090225          if  noMonths >= MONTHSINYEAR;
020100090225             noMonths = MONTHSINYEAR;
020200090225             partEmp = *blank;
020300090305             empcnt += 1;
020400090305             empsal += det_amt(1);
020500090225            else;
020600090225             partEmp = 'P';
020700090305             empOthcnt += 1;
020800090305             empOthsal += det_amt(1);
020900090225          endif;
021000090304
021100090304          return noMonths;
021200090225
021300090225      /end-free
021400090225     P getEmpData      E
021500090304
021600090304
021700090304     P*-----------------------------------------
021800090304     P* Procedure name: prepare_batch
021900090304     P* Purpose:        prepare batch
022000090304     P* Returns:        0
022100090304     P* Parameter:      0
022200090304     P*-----------------------------------------
022300090304     P prepare_batch   B
022400090304     D prepare_batch   PI
022500090304
022600090305     D xcnt            s              6  0
022700090304
022800090304      /free
022900090304
023000090304         //  ...create/open file;
023100090304         //----------------------
023200090305           write_to_file('1');
023300090305
023400090305         //  write header record;
023500090305
023600090305           field(1) = 'H';                 // header
023700090305           field(2) = '999888777';         // company tin
023800090305           field(3) = %char(pay_year);
023900090305           field(4) = get_sys_date();
024000090305           field(5) = %char(empcnt);
024100090305           field(6) = %char(empsal);
024200090305           field(7) = %char(empothcnt);
024300090305           field(8) = %char(empothsal);
024400090305           field(9) = %char(empcnt);
024500090305           field(10) = %char(empsal);
024600090305           field(11) = %char(empothcnt);
024700090305           field(12) = %char(empothsal);
024800090305
024900090305           reset gra_line;
025000090305
025100090305           for xcnt = 1 to %elem(field);
025200090305              gra_line += field(xcnt);
025300090305              if xcnt <> 12;
025400090305                gra_line += PIPE;
025500090305              endif;
025600090305           endfor;
025700090305
025800090305           write_to_file('2':gra_line);
025900090304
026000090304         //  write detail records;
026100090304         //------------------------
026200090304
026300090305           for xcnt = 1 to gra_struc.counter;
026400090305
026500090306             // write detail lines
026600090305              write_to_file('2':gra_struc.line(xcnt));
026700090304
026800090305           endfor;
026900090304
027000090304
027100090305          // write footer record;
027200090304
027300090305           field(1) = 'T';
027400090305           field(2) = %char(tot_amt(1));
027500090305           field(3) = '0';
027600090305           field(4) = '0';
027700090305           field(5) = %char(tot_amt(1));
027800090305           field(6) = %char(tot_amt(2));
027900090305
028000090305           reset gra_line;
028100090305
028200090305           for xcnt = 1 to %elem(field);
028300090305              gra_line += field(xcnt);
028400090305              if xcnt <> 6;
028500090305                gra_line += PIPE;
028600090305              endif;
028700090305           endfor;
028800090305
028900090305           write_to_file('2':gra_line);
029000090304
029100090306         // close file
029200090305           write_to_file('3');
029300090304
029400090304      /end-free
029500090304     P prepare_batch   E
029600090304
029700090304     P*-----------------------------------------
029800090304     P* Procedure name: write_to_file
029900090304     P* Purpose:        write to file
030000090304     P* Returns:        0
030100090304     P* Parameter:      0
030200090304     P*-----------------------------------------
030300090304     P write_to_file   B
030400090304     D write_to_file   PI
030500090304     D option                         1    const
030600090304     D data                         100    const varying options(*nopass)
030700090304
030800170210     D/copy QCPYSRC,IFSPR
030900090304     D fd              S             10I 0
031000090304     D wrdata          S            100A
031100090304     D rddata          S            100A
031200090304     D flags           S             10U 0
031300090304     D mode            S             10U 0
031400090304     D Msg             S             50A
031500090304     D Len             S             10I 0
031600090304      /free
031700090304
031800090304       //*************************************************************
031900090304       // writing data to a stream file
032000090304       //****************************************************************
032100090304         select;
032200090304           when option = '1';
032300090304
032400090304             fd = unlink(filename);
032500090304
032600090304             flags = O_WRONLY + O_CREAT + O_TRUNC +
032700090304                     O_TEXTDATA + O_CODEPAGE;
032800090304
032900090304             mode =  S_IRUSR + S_IWUSR
033000090304                         + S_IRGRP + S_IROTH;
033100090304
033200090304             // Open IFS file in ASCII format
033300090304             fd = open(filename: flags: mode: 819);
033400090304
033500090304             if  fd < 0;
033600090304             // Msg = 'open(): failed for writing';
033700090304             // return;
033800090304             endif;
033900090304
034000090304             // Close IFS file
034100090304             callp  close(fd);
034200090304
034300090304             // Reopen FS file
034400090304             fd = open(filename:O_WRONLY + O_TEXTDATA);
034500090304
034600090304           // Write some data
034700090304           when option = '2';
034800090304             wrdata = data + CRLF;
034900090304             callp write(fd: %addr(wrdata): %len(%trimr(wrdata)));
035000090304
035100090304           // close the file
035200090304           when option = '3';
035300090304             callp  close(fd);
035400090304         endsl;
035500090304
035600090304      /end-free
035700090304     P write_to_file   E
035800090304
035900090304     P*-----------------------------------------
036000090304     P* Procedure name: write_batch_detail
036100090304     P* Purpose:        write batch detail lines
036200090304     P* Returns:        0
036300090304     P* Parameter:      0
036400090304     P*-----------------------------------------
036500090305     P write_batch_detail...
036600090304     P                 B
036700090304     D write_batch_detail...
036800090304     D                 PI
036900090305
037000090305     D xcnt            s              3  0
037100090304      /free
037200090305             //write detail lines
037300090305             if %xfoot(det_amt) <> *zeros;
037400090305
037500090305               field(1) = 'D';
037600090305               field(2) = %trim(last_ird);
037700090305               field(3) = %char(getEmpData);
037800090305               field(4) = partemp;
037900090305               field(5) = '0';
038000090305               field(6) = '0';
038100090305               field(7) = %char(det_amt(1));
038200090305               field(8) = '0';
038300090305               field(9) = '0';
038400090305               field(10) = %char(det_amt(1));
038500090305               field(11) = %char(det_amt(2));
038600090305
038700090305               gra_struc.counter += 1;
038800090305               for xcnt = 1 to %elem(field);
038900090305                 gra_struc.line(gra_struc.counter) += field(xcnt);
039000090305                 if xcnt <> 11;
039100090305                   gra_struc.line(gra_struc.counter) += PIPE;
039200090305                 endif;
039300090305               endfor;
039400090305
039500090305
039600090305             endif;
039700090304
039800090304      /end-free
039900090304     P write_batch_detail...
040000090304     P                 E
040100090304

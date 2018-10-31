000100980103     H DECEDIT('.') DATEDIT(*ymd/)
000200950329     F* SELECTING PAYROLL DEDUCTIONS
000300951104     F* PROGRAMMER:- *** S.BRITTON
000400010829     Fpayhisl01 Ip   e           k disk
000500010423     Fpaytranl01if   e           k disk
000600010423     Fpaycrosl01if   e           k disk
000700980103     **
000800980103     D arr             s             40    dim(2) ctdata perrcd(1)
000900090415     D txt             c                   'PAYROLL DEDUCTION'
001000090415     D txt2            c                   'PAYROLL ALLOWANCE'
001100990218     **
001200050429     Dbill_num         s              9  0
001300050504     Ddesc#            s             30
001400050429      //  data structure for parameter
001500050429     dprm_rec        e ds                  extname(gen_plist)
001600980103     **
001700111002      /copy QCPYSRC,Apppr
001800111002      /copy QCPYSRC,emppr
001900980103     D                sds
002000980103     D  @pgm             *proc
002100111002     D  @user                358    367
002200980112     **
002300050429      /free
002400050429       // Create Bill number from date
002500050429          bill_num = %int(%subst(%editc(phispdate:'X'):7:2) +
002600050429                    %subst(%editc(phispdate:'X'):5:2) +
002700050429                    %subst(%editc(phispdate:'X'):1:4));
002800050429          gen_paye = phisemp;
002900050429
003000050429       // Process detail transactions
003100050429       setll (phisemp:phispdate:phisrdate:phistype)  ptisfmt;
003200050429       reade (phisemp:phispdate:phisrdate:phistype)  ptisfmt;
003300050429
003400050429       dow  not(%eof);
003500050429         chain  ptrntcode  pcrsfmt;
003600050429         if  %found;
003700050429
003800090415              get_payty_info('RD':ptrntcode:payty_struc);
003900090415
004000090415              if  ptyrtype = 'E';
004100090415                desc# = txt2;
004200090415                gen_amt = ptrnamt;
004300090415                 else;
004400090415                desc# = txt;
004500090415                gen_amt = ptrnamt * -1;
004600090415              endif;
004700090415
004800090415            //desc# = txt;
004900090415            //gen_amt = ptrnamt * -1;
005000050429              gen_type = pcrstype;
005100050429              gen_stype = pcrsstype;
005200050429
005300050429              write_x_trans(%addr(prm_rec):phisrdate:bill_num:desc#:
005400050429                              'PAY':*zeros:'E');
005500050429
005600050429               // update  emp files
005700050429              application_up(bill_num:'PAY':phisprno:'I');
005800050429
005900050429         endif;
006000010423
006100050429        reade (phisemp:phispdate:phisrdate:phistype)  ptisfmt;
006200050429       enddo;
006300050429      /end-free
006400980124     **

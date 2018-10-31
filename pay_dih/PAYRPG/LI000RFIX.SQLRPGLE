000100941121     H DATEDIT(*DMY)
000200140820     H bnddir('DISDIR') dftactgrp(*no) actgrp(*new)
000300980119     H* Programmer S.Britton
000400021006     F* Liability master file
000500021113     Fliabm     ip   e           k disk
000600140820
000700140820
000800140820         // Find and fix liability balances
000900140820       Dcl-Pr li000rfix    extpgm('LI000RFIX');
001000140820         Input_period    Packed(6:0)     const;
001100140820         Limit_period    Packed(6:0)     const;
001200140820       End-Pr;
001300140820
001400140820       Dcl-Pi li000rfix;
001500140820         Input_period    Packed(6:0)     const;
001600140820         Limit_period    Packed(6:0)     const;
001700140820       End-Pi;
001800140820
001900140820       Dcl-S wrk_period          Packed(6:0);
002000140820       Dcl-S Month               Packed(2);
002100140820       Dcl-S Balance             Packed(11:2);
002200140820       Dcl-S transaction_total   Packed(11:2);
002300140820       Dcl-S la                  Packed(11:2) dim(12);
002400140820
002500140820      /copy qcpysrc,genpr
002600140820
002700980115     Ilifmt
002800980115     I              liamt01                     la(01)
002900980115     I              liamt02                     la(02)
003000980115     I              liamt03                     la(03)
003100980115     I              liamt04                     la(04)
003200980115     I              liamt05                     la(05)
003300980115     I              liamt06                     la(06)
003400980115     I              liamt07                     la(07)
003500980115     I              liamt08                     la(08)
003600980115     I              liamt09                     la(09)
003700980115     I              liamt10                     la(10)
003800980115     I              liamt11                     la(11)
003900980115     I              liamt12                     la(12)
004000140820
004100140820
004200140820           // Terminater if limit period greater that start
004300140820           if limit_period > input_period;
004400140820             *inlr = *on;
004500140820             return;
004600140820           endif;
004700140820
004800140820           // initialize work period for each record processed
004900140820           wrk_period = input_period;
005000140820           month = %int(%subst(%editc(wrk_period:'X'):5:2));
005100140820
005200140820           // initialize balance to master file data
005300140820           balance = la(month);
005400140820
005500140820           dow wrk_period >= limit_period;
005600140820             update_balance(wrk_period:balance:litype:listyp:liemp);
005700140820             balance = get_period_balance(wrk_period:litype:listyp:liemp);
005800140820             transaction_total = sum_transactions(wrk_period:litype
005900140820                               :listyp:liemp);
006000140820             balance -= transaction_total;
006100140820             wrk_period = get_new_prd(wrk_period:1:'S');
006200140820           enddo;
006300140820
006400140820
006500140820
006600140820       // Get Period Balance     //
006700140820       //........................//
006800140820       Dcl-Proc Get_period_balance;
006900140820         Dcl-Pi Get_period_balance packed(11:2);
007000140820            EndPeriod    Packed(6) Const;
007100140820            B_type    Packed(2) Const;
007200140820            B_styp    Packed(3) Const;
007300140820            B_emp     Packed(6) Const;
007400140820         End-pi;
007500140820
007600140820       Dcl-S EndBalance         Packed(11:2);
007700140820
007800140820
007900140820          exec sql
008000140820              select lbamt into :EndBalance from  liabm_bal
008100140820                  where  lbtype = :B_type  and lbstyp = :B_styp
008200140820                  and  lbemp = :B_emp  and lbprno = :EndPeriod;
008300140820
008400140820            return EndBalance;
008500140820
008600140820
008700140820       End-Proc Get_period_balance;
008800140820
008900140820
009000140820       // Sum Transactions       //
009100140820       //........................//
009200140820       Dcl-Proc Sum_Transactions;
009300140820         Dcl-Pi Sum_Transactions packed(11:2);
009400140820            Period    Packed(6) Const;
009500140820            S_type    Packed(2) Const;
009600140820            S_styp    Packed(3) Const;
009700140820            S_emp     Packed(6) Const;
009800140820         End-pi;
009900140820
010000140820       Dcl-S TotalValue      Packed(11:2);
010100140820       Dcl-S Ind             Int(5);
010200140820
010300140820
010400140820             Exec Sql
010500140820                select sum(ltamt) into :totalvalue :Ind from
010600140820                 LIABTL06  where  lttype = :S_type and
010700140820                    ltemp = :S_emp and ltstyp = :S_styp
010800140820                    and ltprno = :period;
010900140820
011000140820            return totalvalue;
011100140820
011200140820       End-Proc Sum_Transactions;
011300140820
011400140820
011500140820       // update     Balance     //
011600140820       //........................//
011700140820       Dcl-Proc update_balance;
011800140820         Dcl-Pi update_balance;
011900140820            UpdatePeriod    Packed(6) Const;
012000140820            PeriodBalance   Packed(11:2) Const;
012100140820            U_type    Packed(2) Const;
012200140820            U_styp    Packed(3) Const;
012300140820            U_emp     Packed(6) Const;
012400140820         End-pi;
012500140820
012600140820          exec sql
012700140820              update liabm_bal  set lbamt  = :PeriodBalance
012800140820                  where  lbtype = :U_type  and lbstyp = :U_styp
012900140820                  and  lbemp = :U_emp  and lbprno = :UpdatePeriod
013000140820                  with NC;
013100140820
013200140820       End-Proc update_balance;
013300140820

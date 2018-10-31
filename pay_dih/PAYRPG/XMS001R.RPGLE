000100171024     H DECEDIT('.') DATEDIT(*DMY/) dftactgrp(*no) bnddir('DISDIR')
000200171024     H alwnull(*usrctl)
000300090620      //*
000400090620      //* EDITING ABSENTEEISM FOR XMAS BONUS
000500090620      //* MODIFIED BY T.I. BYNOE -- 26/10/78
000600090620      //* MOD.  S.BRITTON  - 07/1995 , 06/2009
000700021125     Fempmast   ip   e           k DISK
000800010216     Fabsfl02   if   e           k DISK
000900010216     Fabstypl01 if   e           k DISK
001000040622     Fhmrcon    if   e             DISK    usropn
001100010216     Fabssum    o    e             disk    usropn
001200090620      // Payroll PROTOTYPES
001300110706      /copy qcpysrc,PAYPR
001400110706      /copy qcpysrc,genpr
001500090620
001600090620     D cmdexc          pr                  extpgm('QCMDEXC')
001700090620     D cmd                          200    options(*varsize) const
001800090620     D cmdlen                        15p 5 const
001900090620
002000090620
002100010216     D                 ds                  inz
002200010216     D  wrk_period                    6  0
002300010216     D  wrk_year                      4  0 overlay(wrk_period)
002400040622     D  wrk_mth                       2  0 overlay(wrk_period:5)
002500010216     D                 ds                  inz
002600010216     D  end_period                    6  0
002700010216     D  end_year                      4  0 overlay(end_period)
002800040622     D  end_mth                       2  0 overlay(end_period:5)
002900090620
003000010216     D  tot_days       s              4  1
003100040622     D  date           s              8  0
003200040622     D  prno           s              6  0
003300090620     D  xmseq#         s              2  0 dtaara(XMSSEQCHK)
003400090620     D  command        s             20    inz('CLRPFM ABSSUM')
003500090620
003600981217     D                SDS
003700981217     D  @PGM             *PROC
003800981217     D  @USER                254    263
003900090620
004000090620
004100090620      /FREE
004200090620        if not *in99;
004300010216
004400090620          cmdexc(command:%size(command));
004500090620
004600090620          open abssum;
004700090620          open  hmrcon;
004800090620          read  hmrcon;
004900090620          close hmrcon;
005000090620
005100090620          get_sys_date(date);
005200090620          get_prd_info('*prd':*blanks:prno:date);
005300040622
005400040623           if %int(%subst(%char(prno):5:2)) < hcabssmth;
005500040622             wrk_year = *year - 1;
005600091125            else;
005700091125             wrk_year = *year - 1;
005800091125          // wrk_year = *year;
005900040622           endif;
006000040622
006100091125           wrk_mth = hcabssmth;
006200040623
006300091125           end_period = get_new_prd(wrk_period:11:'A');
006400090619
006500090620          *in99 = '1';
006600090620        endif;
006700091125
006800090620
006900090620        in *lock *dtaara;
007000090620        xmseq# = 1;
007100090620        out xmseq#;
007200090620
007300090620        if get_cat(status) = 'A';
007400090620          setll *loval abtfmt;
007500090620          read abtfmt;
007600090620
007700090620          dow not(%eof(abstypl01));
007800090620            // Select absenteeism types used for accruing penalties
007900090620            if abtflag = 'Y';
008000090620              setll (emp:abttype:wrk_period) absfmt;
008100090620              reade (emp:abttype) absfmt;
008200090620
008300090620              dow not %eof(absfl02);
008400090620
008500090620               // if period is greater than current period - leave data gathering Loop
008600090620               if absprno > end_period;
008700090620                 leave;
008800090620               endif;
008900090620
009000090620               // Summarize all absenteeism days
009100090620               exsr sum_days;
009200090620
009300090620               reade (emp:abttype) Absfmt;
009400090620              Enddo;
009500090620
009600090620            Endif;
009700090620
009800090620           read Abtfmt;
009900090620          Enddo;
010000090620
010100090620           if tot_days > 0;
010200090620             write abssrec;
010300090620             clear abssrec;
010400090620             clear tot_days;
010500090620           endif;
010600021125
010700090620        endif;
010800010216
010900090620         //*******************************************************
011000090620         // Summarize absent days for different periods
011100090620         //*******************************************************
011200090620         begsr sum_days;
011300101206
011400101206           tot_days += absdays;
011500101206
011600090620           select;
011700090620           when abstype = 1;
011800090620             totawl += absdays;
011900010216
012000090620           when abstype = 2;
012100090620             totawo += absdays;
012200010216
012300090620           when abstype = 3;
012400090620             totind += absdays;
012500010216
012600090620           when abstype = 4;
012700090620             totjur += absdays;
012800010216
012900090620           when abstype = 5 or abstype = 7;
013000090620             totsic += absdays;
013100010216
013200090620           when abstype = 6;
013300090620             totsus += absdays;
013400101206
013500101206           when abstype = 8;
013600101206             totlat += absdays;
013700010216
013800090620           other;
013900090620             totsic += absdays;
014000090620           endsl;
014100090620
014200090620         endsr;
014300010216
014400090620      /END-FREE

000100170220     H DATEDIT(*DMY) alwnull(*usrctl)
000200090415      // Altered by Chris Williams
000300090415      // 97/07/14
000400090415      // LEAVE TRANSACTION FILE
000500011010     Flevfl02   uf   e           k disk    commit
000600011024     Fpayrtypl01if   e           k disk
000700020111     Fpayrdefl03if   e           K DISK
000800010911     Fvacation  o    e           K disk    commit
000900090415      // Employee Master File
001000010706     Fempmast   if   e           k disk
001100090415      // Employee Status Master File
001200010706     Femptypel02if   e           k disk
001300090415      //
001400950530     D                SDS
001500950530     D  @PGM             *PROC
001600950530     D  @USER                254    263
001700090415      //
001800010716     D run_type        s              1
001900010717     D trns_code       s              2  0 inz(01)                              Normal wages
002000010717     D counter         s              3  0                                      Normal wages
002100010717     D calc_hrs_pgm    c                   const('PAYCALCLV2')                  Normal wages
002200090415      //
002300970714     C     *entry        plist
002400010716     C                   parm                    run_type
002500090415      //
002600010716     C     get_key       klist
002700010716     C                   kfld                    run_type
002800010716     C                   kfld                    trns_code
002900090415      //
003000090415      /FREE
003100090415       chain run_type prtyfmt;
003200090415       chain get_key prdffmt;
003300090415       //
003400090415       if %found;
003500090415         exsr @load;
003600090415         exsr @calchrs;
003700090415       else;
003800090415         *inlr = *on;
003900090415       endif;
004000090415       //
004100090415       setll *loval emprec;
004200090415       return;
004300090415       //
004400090415       //*************************************************************
004500090415       //*  LOAD FILE WITH LEAVE PAYMENT DETAILS .
004600090415       //*************************************************************
004700090415       //
004800090415       begsr @load;
004900090415         //  Read file to employee no
005000090415         setll prtyldate levfmt;
005100090415         read levfmt;
005200090415         *INlr = %EOF;
005300090415         dow not(*inlr);
005400090415           //  Chain file to get data
005500090415           //
005600090415           chain levemp emprec;
005700090415           if %found;
005800090415             exsr @file;
005900090415           endif;
006000090415           //
006100090415           read levfmt;
006200090415           *INlr = %EOF;
006300090415           //* when the end date  equals start date end read
006400090415           //*
006500090415      /END-FREE
006600010913     C  nlrlevsdate      comp      prtyndate                          lr  lr
006700090415      /FREE
006800090415         enddo;
006900090415         //
007000090415       endsr;
007100090415       //
007200090415       //*************************************************************
007300090415       //***       CREATE PAY MASTER FILE                       ******
007400090415       //*************************************************************
007500090415       BEGSR @FILE;
007600090415         //
007700090415         if levsts = *blank;
007800010706
007900090415           chain status empfmt;
008000090415           //
008100090415           if emppyopt = 'H';
008200090415             vacemp = emp;
008300090415             vacdept = dept;
008400090415             vacsdate = levsdate;
008500090415             vacedate = levedate;
008600090415             write(E) vacfmt;
008700090415             *IN99 = %ERROR;
008800090415             counter = counter + 1;
008900090415           endif;
009000090415           //
009100090415           levsts = 'Y';
009200090415           update levfmt;
009300090415           //
009400090415         endif;
009500090415         //
009600090415       endsr;
009700090415       //*************************************************************
009800090415       //*  Calculate Leave Payment Hours.
009900090415       //*************************************************************
010000090415       //
010100090415       begsr @calchrs;
010200090415         if counter <> *zeros;
010300090415      /END-FREE
010400010717     C                   callb     calc_hrs_pgm
010500010816     C                   parm                    run_type
010600010717     C                   parm                    prtyldate
010700010717     C                   parm                    prtyndate
010800010717     C                   parm                    prtycycode
010900090415      /FREE
011000090415         endif;
011100090415         //
011200090415       endsr;
011300090415       //**************************************************************
011400090415      /END-FREE

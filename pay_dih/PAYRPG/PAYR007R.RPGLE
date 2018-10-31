000100950530     H DATEDIT(*DMY)
000200090415      // Altered by Chris Williams
000300090415      // 97/07/14
000400090415      // LEAVE TRANSACTION FILE
000500010215     Flevfl02   uf   e           k disk
000600970714     Fvac       o    e           K DISK
000700090415      // Special payroll payment file
000800970714     Fswtbon    o    e           K DISK
000900090415      // BANKS DIH Employee Master File
001000970714     Fempmasl3  IF   E           K DISK
001100090415      //
001200090415      //
001300010215     dfdate            s               d   datfmt(*iso)
001400010215     dldate            s               d   datfmt(*iso)
001500010221     dldate2           s               d   datfmt(*ymd)
001600950530     D                SDS
001700950530     D  @PGM             *PROC
001800950530     D  @USER                254    263
001900090415      //
002000970714     C     *entry        plist
002100970714     C                   parm                    pay_type          1 0
002200090415      //
002201090415      /FREE
002300090415       in *lock *dtaara;
002301090415      /END-FREE
002400970714     c     *dtaara       define                  fndate            8
002500010215     c     *ymd          move      fndate        fdate
002600010215     c                   move      fdate         sdate             8 0
002700090415      //
002800970715     c     fdate         adddur    13:*d         ldate
002900090415      //
003000010228     c     *iso          move      ldate         edate             8 0
003100970715     c                   adddur    1:*d          ldate
003200010221     c                   move      ldate         ldate2
003300010221     c                   move      ldate2        fndate
003301090415      /FREE
003400090415       out fndate;
003500090415       //
003600090415       exsr @load;
003700090415       //
003800090415       //
003900090415       //**************************************************************
004000090415       //*  LOAD FILE WITH LEAVE PAYMENT DETAILS .
004100090415       //**************************************************************
004200090415       //****************************************************************
004300090415       //
004400090415       begsr @load;
004500090415         //  Read file to employee no
004600090415         setll sdate levfmt;
004700090415         read levfmt;
004701090415         *IN55 = %EOF;
004800090415         DOW *in55 <> *on;
004900090415           //  Chain file to get data
005000090415           //
005100090415           chain levemp emprec;
005101090415           *IN88 = NOT %FOUND;
005200090415           if not(*in88);
005300090415             weeks = *zeros;
005400090415             if levtype = 'W1' or levtype = 'W2';
005500090415               weeks = levnum;
005600090415             else;
005700090415               if levtype = ' M';
005800090415                 weeks = 4;
005900090415               endif;
006000090415             endif;
006100090415             exsr @file;
006200090415           endif;
006300090415           //
006400090415           read levfmt;
006401090415           *IN55 = %EOF;
006500090415           //* when the end date  equals start date end read
006600090415           //*
006601090415      /END-FREE
006700010221     C  n55edate         comp      levsdate                             55
006701090415      /FREE
006800090415         enddo;
006900090415         //
007000090415         *INlr = *ON;
007100090415         return;
007200090415       endsr;
007300090415       //
007400090415       //**************************************************************
007500090415       //***       CREATE PAY MASTER FILE                         ******
007600090415       //**************************************************************
007700090415       BEGSR @FILE;
007800090415         //
007900090415         if levsts = *blank;
008000090415           write(E) vacrec;
008001090415           *IN99 = %ERROR;
008100090415           chain emp emprec;
008101090415           *IN77 = NOT %FOUND;
008200090415           //
008300090415           //
008400090415           // WRITE RECORD FOR Performance Incentive
008500090415           if weeks >= 2;
008501090415      /END-FREE
008600970417     C                   z-add     *zero         amount
008700970417     C                   z-add     1             ptype
008800970417     C                   z-add     2             rtype
008801090415      /FREE
008900090415             write(E) swbrec;
008901090415             *IN99 = %ERROR;
009000090415           endif;
009100090415           //
009200090415           if weeks = 4;
009300090415             // Write Future PI record -
009301090415      /END-FREE
009400970417     C                   z-add     *zero         amount
009500000216     C                   z-add     1             ptype
009600970417     C                   z-add     1             rtype
009601090415      /FREE
009700090415             write(E) swbrec;
009701090415             *IN99 = %ERROR;
009800090415           endif;
009900090415           //
010000090415           levsts = 'Y';
010100090415           update levfmt;
010200090415           //
010300090415         endif;
010400090415         //
010500090415       endsr;
010600090415       //**************************************************************
010601090415      /END-FREE

000100091012      //***  PROGRAMMER - SAMUEL BRITTON
000200091012      //***  Populating Citizen Bank transaction file
000300091012      //  Flag Parameter values --- *off - detail transactions
000400091012      //                        --- *on - Last detail transaction
000500091012      //                                  for batch
000600000629     Ptrans_up         b
000700091012      //-------------------------------------------------------------**
000800000629     Dtrans_up         pi
000900000630     Dciti_t#                              like(citi_t)
001000000629     Dciti_a#                              like(citi_a)
001100000630     Damount                         12  2
001200000630     Ddate_crd                        6  0
001300000708     Dcount                           6  0
001400000630     Dtrn_type                             like(winsta)
001500000630     Dlast_trn_flag                    n
001600000717     Dbatch                           4  0
001700091012
001800091012      //-------------------------------------------------------------**
001900000630     Dciti_t         e ds                  extname(citibtch)
002000000630     Dciti_a         e ds                  extname(citidefn)
002100000629     Dcrdate           s               d   datfmt(*dmy)
002200000629     Dpydate           s               d   datfmt(*dmy)
002300091012     Dtrn_txt          c                   'Salary Credit'
002400091012     Dtrn_txt2         c                   'Salary Paid'
002500091012     Dtrn_txt1         c                   'Shares Dividend Credit'
002600091012     Dtrn_txt21        c                   'Shares Dividend Paid'
002700100127     Dtrn_txt11        c                   'Discount Cheque Credit'
002800100127     Dtrn_txt22        c                   'Discount Cheque Paid'
002900000629     Dcentury          s              1
003000091012      //
003100091012      //*********************************************************************
003200091012      //**  PROCESSING BANK"S BATCH INFORMATION ***
003300091012      //*********************************************************************
003400091012      //
003500091012      //
003600000630     C                   move      citi_t#       citi_t
003700000630     C                   move      citi_a#       citi_a
003800000630     C     *dmy          move      date_crd      crdate
003900000630     C                   move      trn_type      winsta
004000000630
004100091012      // write transaction information
004200091012      /FREE
004300091012       select;
004400091012         when trn_type = 'T';
004500091012      /END-FREE
004600000629     C                   move      '01'          winobk
004700000629     C                   z-add     *zeros        winobr
004800000629     C                   move      '01'          winbnk
004900000629
005000091012      // get accounting info.
005100000629     C                   z-add     acmbrn        winbrn
005200000629     C                   move      acmccy        winccy
005300000629     C                   z-add     acmgln        wingln
005400000629     C                   z-add     acmccn        winccn
005500000629     C                   move      acmaty        winaty
005600000629     C                   z-add     acmcun        wincun
005700000629
005800000630     C                   z-add     acmacc        winacc
005900091012      //
006000000629     C                   extrct    crdate:*m     winvdm
006100000629     C                   extrct    crdate:*d     winvdd
006200000629     C                   extrct    crdate:*y     winvdy
006300091012
006400091012      /FREE
006500091012         if last_trn_flag;
006600091012
006700091012           select;
006800091012             when origin = 'PAY';
006900091012                wintds = trn_txt2;
007000091012             when origin = 'SHR';
007100091012                wintds = trn_txt21;
007200100125          //----To see if this is needed--------------//
007300100127               when origin = 'CRD';
007400100127                  wintds = trn_txt22;
007500091012           endsl;
007600091012           windra = amount;
007700091012           wincr1 = *zeros;
007800091012           wintcd = 'DR';
007900091012
008000091012         else;
008100091012           wintcd = 'CR';
008200091012           select;
008300091012             when origin = 'PAY';
008400091012                wintds = trn_txt;
008500091012             when origin = 'SHR';
008600091012                wintds = trn_txt1;
008700100127             when origin = 'CRD';
008800100127                wintds = trn_txt11;
008900091012           endsl;
009000091012           wincr1 = amount;
009100091012           windra = *zeros;
009200091012         endif;
009300091012         //
009400091012      /END-FREE
009500000717     C                   z-add     batch         winkbt
009600000629     C                   z-add     count         winksq
009700000706     C                   z-add     *zeros        winfer
009800091012      // write batch date
009900000629     C                   extrct    crdate:*m     winbdm
010000000629     C                   extrct    crdate:*d     winbdd
010100000629     C                   extrct    crdate:*y     winbdy
010200091012
010300091012      /FREE
010400091012         century = (%subst(%char(*year):2:2));
010500091012      /END-FREE
010600000629     C                   move      century       winbdc
010700000629     C                   move      century       winvdc
010800000629
010900091012      // writing header information
011000091012      /FREE
011100091012         when trn_type = 'B';
011200091012
011300091012      /END-FREE
011400000630     C                   move      'GYD'         winccy
011500091012      /FREE
011600091012         windra = amount;
011700091012         wincr1 = amount;
011800091012
011900091012      /END-FREE
012000000630     C                   move      *blanks       wintds
012100000630     C                   z-add     *zeros        winvdm
012200000630     C                   z-add     *zeros        winvdd
012300000630     C                   z-add     *zeros        winvdy
012400000630     C                   z-add     *zeros        winksq
012500000720     C                   z-add     *zeros        winacc
012600000720     C                   move      *blanks       wintcd
012700000630     C                   move      'A'           winarf
012800000630     C                   z-add     count         winttr
012900091012
013000091012      /FREE
013100091012       endsl;
013200091012
013300091012      /END-FREE
013400000630     C                   move      citi_t        citi_t#
013500000630     C                   move      citi_a        citi_a#
013600091012      //
013700000629     Ptrans_up         e

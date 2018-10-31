000100980218     H DATEDIT(*ymd)
000200980218     F****Programmer - W.Robinson
000300980218     Fnamupd    if   E           K DISK
000400980218     Fjrnml02   if   e           k disk
000500980218     Freqprtf   o    f   80        printer oflind(*inof)
000600970325     Dtimeeur          s               d   datfmt(*eur)
000700980218     D                sds
000800980218     D  @pgm             *proc
000900980218     D  @n_prm           *parms
001000980218     D  @user                254    263
001100970207     **
001200980218     c     *entry        plist
001300980218     c                   parm                    jrnum#            9 0
001400980218     **
001500980401     **** get sys date
001600980401     C                   call      '@GETSYSDTE'
001700980401     C                   parm                    @date             8 0
001800980218     c     *like         define    jramt         ltamt#
001900980218     c     nmkey         klist
002000980218     c                   kfld                    jracc
002100980218     c                   kfld                    jrspc
002200970325     **
002300980218     C     jrnum#        chain     jrfmt                              55
002400980218     C                   dow       not(*in55)
002500980218     c     nmkey         chain     nmrec                              52
002600980218     **
002700980218     c                   add       1             chk               2 0
002800980218     c                   if        chk = 1
002900980218     C                   movel     nmdsp         debit            15
003000980218     C                   z-add     jremp         debled            4 0
003100980218     C                   z-add     jracc         debacc            6 0
003200980218     c                   else
003300980218     C                   movel     nmdsp         credit           15
003400980218     C                   z-add     jremp         crtled            4 0
003500980218     C                   z-add     jracc         crtacc            6 0
003600980218     C                   endif
003700980218     c     jrnum#        reade     jrfmt                                  55
003800980218     C                   enddo
003900980218     C                   z-add     jramt         ltamt#
004000980218     C                   movel     jrtext        lttext#          30
004100980218     C                   except
004200980218     c                   seton                                            lr
004300980218     *************************************************************
004400980218     ** receipt output                                           *
004500980218     *************************************************************
004600980218     Oreqprtf   e                           1 01
004700980218     O                       @pgm                10
004800980218     O                                           46 'BANKS DIH LTD'
004900980218     **
005000980218     Oreqprtf   e                           2
005100990217     O                                           43 'Transaction'
005200980218     O                                           68 '          No...:'
005300980218     O                       jrnum#        z     80
005400980218     **
005500980218     Oreqprtf   e                           3
005600980219     O                                           68 'Date..:'
005700980401     O                       @date               80 '    /  /  '
005800980219     O          e                        1
005900980219     O                                           68 'Period:'
006000980219     O                       jrprno              80
006100980218     O          e                        1
006200980218     O                                           40 'DEBIT'
006300980218     O                                           80 'CREDIT'
006400980218     O          e                        1
006500980218     O                                           16 'Account..:'
006600980218     O                       debit               40
006700980218     O                       credit              80
006800980218     O          e                        1
006900980219     O                                           16 'Ledger.........:'
007000980218     O                       debled              40
007100980218     O                       crtled              80
007200980218     O          e                        1
007300980219     O                                           16 'Account No.....:'
007400980218     O                       debacc              40
007500980218     O                       crtacc              80
007600980218     **
007700980218     O          e                        2  2
007800980219     O                                           16 'Particulars....:'
007900980218     O                       lttext#            + 2
008000980218     **
008100980219     O                                          + 4 'Amount..:'
008200980219     O                       ltamt#        1     80
008300980218     **
008400980218     O          e                        3
008500980218     O                                           16 ' Authorised By.:'
008600980218     O                                          + 1 '________________'

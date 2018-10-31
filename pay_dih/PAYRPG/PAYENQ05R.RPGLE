000100950911     H* ********************************************************
000200010920     H*              D I S E R V  S Y S T E M S .              *
000300010412     H*                Payroll Control System                  *
000400950911     H*                                                        *
000500950911     H*                         BY                             *
000600950911     H*                                                        *
000700010412     H*       DISERV APPLICATION DEVELOPMENT TEAM              *
000800970115     H*             P.O BOX 10140, GEORGETOWN,GUYANA           *
000900950911     H*                                                        *
001000950911     H*                                                        *
001100010412     H*       RPG PROGRAM NAME..: PAYENQ01R                    *
001200950911     H*                                                        *
001300970724     H*       PROGRAMMER NAME: CHRIS WILLIAMS                  *
001400010412     H*       DESIGN DATE....: 2001/04/12                      *
001500950911     H*                                                        *
001600010920     H*  PROGRAM DESCRIPTION: Payroll Date Enquiry             *
001700950911     H*                                                        *
001800010412     H*  THIS PROGRAM IS DESIGNED TO DSPLAY THE PAYROLL        *
001900010920     H*  Dates FILE( From The PAYHIS  Master)                  *
002000950911     H*                                                        *
002100950911     H*                                                        *
002200950911     H*                                                        *
002300950911     H**********************************************************
002400111002     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
002500111002     H/define profoundui
002600011024     Fpayrtypl01if   e           k disk
002700010920     F*ayhisl09 if   e           k disk
002800010920     Fpayhisl08 if   e           k disk
002900010920     Fpayenq05d cf   e             workstn sfile(winrec:rrn)
003000111002     F/IF DEFINED(WEB)
003100111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003200111002     F/ENDIF
003300950911     D*
003400970117     D                sds
003500970117     D  @pgm             *proc
003600111002     D  @user                358    367
003700010412     D  @parm            *parms
003800111002
003900010917     Dprd              s              6  0
004000010917     Dpay_date         s              8  0
004100010920     Dpay_type         s              1
004200010920     Dphisndate##      s              8  0
004300010920     Drrn              s              4  0
004400950920     ** Execute subroutine to show header
004500970724     C     *entry        plist
004600010920     c                   parm                    pay_type
004700010920     c                   parm                    prd
004800010917     c                   parm                    pay_date
004900970724     c
005000010412
005100010913     C     prd_key       klist
005200010917     c                   kfld                    prd
005300010913     c                   kfld                    prtyptype
005400010913     c
005500010412     ** if used to get pay code allow selection
005600010925     C                   if        @parm   > 0
005700010412     C                   eval      *in45 = '1'
005800010412     C                   endif
005900970116     **
006000010412     C                   dow       not *inlr
006100970725     C                   move      '1'           *in13
006200970725     C                   write     window
006300970725     C                   write     wdwctl
006400970725     C                   move      '0'           *in13
006500010920     C                   eval      rrn = 0
006600010412
006700010913     C     *loval        setll     prtyfmt
006800010913     C                   read      prtyfmt                                21
006900030509     **
007000010913     C                   dow       not *in21
007100010920     C     prd_key       setll     phisfmt
007200010920     C     prd_key       reade     phisfmt                                20
007300010920     **
007400030509     C                   if        not(*in20)
007500030509     C                   eval      sel# = ' '
007600030509     C                   eval      phisndate## = phisndate
007700030509     **
007800010913     C                   dow       not *in20
007900970117     **
008000010920
008100010920
008200030509     C                   if        phisndate## <> phisndate
008300010920     C                   eval      rrn = rrn + 1
008400970116     C                   write     winrec
008500010920     C                   eval      phisndate## = phisndate
008600010913     C                   endif
008700030509
008800030509     C                   eval      phistype# = phistype
008900030509     C                   eval      phisndate# = phisndate
009000030509     C                   eval      phispdate# = phispdate
009100010917
009200010913     C     prd_key       reade     phisfmt                                20
009300010913     c                   enddo
009400030509     C                   eval      rrn = rrn + 1
009500030509     C                   write     winrec
009600030509     C                   endif
009700010917
009800010913     C                   read      prtyfmt                                21
009900010913     c                   enddo
010000970725     **
010100970725     C                   write     window
010200970116     C                   exfmt     wdwctl
010300970723     ** F3 to end pgm
010400010412     c                   if        *in03
010500010412     c                   eval      *inlr = '1'
010600970723     c                   return
010700970723     c                   endif
010800010412
010900970724     C                   readc     winrec                                 55
011000970724     C                   dow       not(*in55)
011100970724     **
011200970724     ** If Selection is made, Return To Calling Pgm
011300970724     **
011400010412     C                   if        sel# = 'X'
011500010920     c                   eval      pay_date = phisndate#
011600010920     c                   eval      pay_type = phistype#
011700970724     c                   seton                                            lr
011800970724     c                   return
011900010412     C                   endif
012000970724     ***
012100970724     C                   readc     winrec                                 55
012200970724     C                   enddo
012300970723     **
012400970116     C                   enddo
012500970724     **

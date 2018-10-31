000100950911     H* ********************************************************
000200950911     H*              B A N K S    D I H    L T D.              *
000300010412     H*                Payroll Control System                  *
000400950911     H*                                                        *
000500950911     H*                         BY                             *
000600950911     H*                                                        *
000700010412     H*       DISERV APPLICATION DEVELOPMENT TEAM              *
000800970115     H*             P.O BOX 10140, GEORGETOWN,GUYANA           *
000900950911     H*                                                        *
001000950911     H*                                                        *
001100010904     H*       RPG PROGRAM NAME..: PAYENQ04R                    *
001200950911     H*                                                        *
001300970724     H*       PROGRAMMER NAME: CHRIS WILLIAMS                  *
001400010904     H*       DESIGN DATE....: 2001/09/04                      *
001500950911     H*                                                        *
001600010904     H*  PROGRAM DESCRIPTION: Cycle Code Enquiry               *
001700950911     H*                                                        *
001800010412     H*  THIS PROGRAM IS DESIGNED TO DSPLAY THE PAYROLL        *
001900010904     H*  CYCLE CODE FILE( From The PAYCYCD Master)             *
002000950911     H*                                                        *
002100950911     H*                                                        *
002200950911     H*                                                        *
002300950911     H**********************************************************
002400111002     H DATEDIT(*DMY) bnddir('DISDIR') dftactgrp(*NO) actgrp(*caller)
002500111002     H/define profoundui
002600010904     Fpaycycd   if   e           k disk
002700010904     Fpayenq04d cf   e             workstn sfile(winrec:rec)
002800111002     F/IF DEFINED(WEB)
002900111002     F                                     HANDLER('PROFOUNDUI(HANDLER)')
003000111002     F/ENDIF
003100950911     D*
003200970117     D                sds
003300970117     D  @pgm             *proc
003400111002     D  @user                358    367
003500010412     D  @parm            *parms
003600010904     D  in_code        s              2
003700950920     ** Execute subroutine to show header
003800970724     C     *entry        plist
003900010904     c                   parm                    in_code
004000970724     c
004100010412
004200010412     ** if used to get pay code allow selection
004300010412
004400010412     C                   if        @parm > 0
004500010412     C                   eval      *in45 = '1'
004600010412     C                   endif
004700970116     **
004800010412     C                   dow       not *inlr
004900970725     C                   move      '1'           *in13
005000970725     C                   write     window
005100970725     C                   write     wdwctl
005200970725     C                   move      '0'           *in13
005300970116     C                   z-add     0             rec
005400010412
005500010904     C     *loval        setll     cycfmt
005600010904     C                   read      cycfmt
005700010904     C                   dow       not(%eof)
005800010412     ** Get code from paytyp Master
005900970117     **
006000010412     C                   eval      sel# = ' '
006100010904     C                   eval      cyccode#= cyccode
006200010904     C                   eval      cycdesc#= cycdesc
006300970116     C                   add       1             rec               4 0
006400970116     C                   write     winrec
006500010904     C                   read      cycfmt
006600970725     c                   enddo
006700010904     c                   eval      *in20 = '1'
006800970725     **
006900970725     C                   write     window
007000970116     C                   exfmt     wdwctl
007100970723     ** F3 to end pgm
007200010412     c                   if        *in03
007300010412     c                   eval      *inlr = '1'
007400970723     c                   return
007500970723     c                   endif
007600010412
007700970724     C                   readc     winrec                                 55
007800970724     C                   dow       not(*in55)
007900970724     **
008000970724     ** If Selection is made, Return To Calling Pgm
008100970724     **
008200010412     C                   if        sel# = 'X'
008300010904     c                   eval      in_code = cyccode#
008400970724     c                   seton                                            lr
008500970724     c                   return
008600010412     C                   endif
008700970724     ***
008800970724     C                   readc     winrec                                 55
008900970724     C                   enddo
009000970723     **
009100970116     C                   enddo
009200970724     **

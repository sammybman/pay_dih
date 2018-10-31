000100960522     H datedit(*dmy)
000200960426
000300960426     H******  PROGRAMMER - SAMUEL BRITTON    *******
000400990108     H******  Verify Time file against system date   ******
000500960426
000600960506      * This program uses a file created by the conversion utility
000700960702      * of Kronos TKC version 8d to produce the payroll file in the
000800960506      * format needed by Banks DiH Limited.
000900960506      * NOTE: Upgrades to newer versions of the Kronos Software might
001000960506      *       necessitate changes to this program.
001100960506
001200980209      * DISPLAY FILE
001300980209     FKRON13D   CF   E             WORKSTN
001400960520      * Kronos payroll data file
001500971209     Ftimef     if   f  256        disk
001600960426     **
001700960427     Dempn             S             10
001800960522     Dkempn            S             10  0
001900960427     Dsposn            S              5  0
002000960427     Depos             S              5  0
002100980710     Dkfndate          S             10
002200970715     **
002300970715     D@msgid           S              7    inz('MSG0042')
002400960427     Daryrec           S              1    DIM(256)
002500960427     Daryscn           S              3  0 DIM(256)
002600980710     **
002700980710     Dfn_date_fld      s               d   datfmt(*ymd)
002800980710     Dkr_date_fld      s               d   datfmt(*usa)
002900980710     **
003000960427     D                sds
003100960426     D  @user                254    263
003200960426     D  @pgm             *proc
003300970715     **
003400970715     D                 DS
003500970715     Dfldhrs                   1      6
003600960427     Dfld1w                    1      3  0
003700960427     Dfld1d                    5      6  2
003800990108     dsysdate          s               d   datfmt(*eur)
003900960216     **
004000960427     Itimef     aa
004100960427     I                                  1  256  trec
004200960427     I                                  1  256  aryrec
004300960427     **
004400960427
004500980209     C     *entry        plist
004600980209     C                   parm                    chk               1
004700990108
004800980710     ** Get fortnight from Kronos file for verification
004900980710     C                   read      timef                                  10
005000990108     **
005100990108     ** Get system date
005200990108     c                   move      timex         sysdate
005300980710     **
005400980710     ** Get current fortnight's selection start date
005500980710     C                   in        *dtaara
005600980710     C     *dtaara       define                  fndate            8
005700980710     C                   eval      kfndate = %subst(trec:2:10)
005800980710     **
005900980710     C                   move      fndate        fn_date_fld
006000980710     C                   move      kfndate       kr_date_fld
006100980710     C     fn_date_fld   subdur    kr_date_fld   numdays:*d        5 0
006200980710     **
006300980710     C                   if        numdays >2
006400980710     C                   call      'ESCPMSG'
006500980710     C                   parm                    @pgm
006600980710     C                   parm      'MSG0048'     @msgid
006700980710     C                   parm      'PAYLIB'      @lib             10
006800980710     C                   parm      'PAYMESS'     @file            10
006900980710     C                   endif
007000980710     C

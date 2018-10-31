000100170209     H  alwnull(*usrctl)
000200990413     F******* Maintain bank account sequence file--- S.BRITTON ******
000300990201     **
000400020410     Fempmast   if   e           k disk
000500990413     Fpaybank   if   e           k disk
000600050211     Fpaybanknetif   e           k disk
000700990413     FpaybankseqO    e             disk    usropn
000800010904     ** Payroll/general PROTOTYPES
000900110829      /copy QCPYSRC,genpr
001000110829      /copy QCPYSRC,PAYPR
001100990114     **
001200950930     D                SDS
001300950930     D  @PGM             *PROC
001400950930     D  @USER                254    263
001500960409     **
001600051118     D mim_type        s              1
001700990413     D CMD             C                   CONST('CLRPFM PAYBANKSEQ')
001800990412     **-------------------------------------------------------------------------
001900990413     C     *entry        plist
002000990413     C                   parm                    pbtype
002100051118     C                   parm                    mim_type
002200990413
002300990413     ** Clear sequence file for new output
002400990413     C     *IN99         IFEQ      '0'
002500990413     C                   z-add     17            LENGTH           15 5
002600990413     C                   MOVE      CMD           COMMAN           17
002700990413     C                   CALL      'QCMDEXC'
002800990413     C                   PARM                    COMMAN
002900990413     C                   PARM                    LENGTH
003000990413     C                   open      paybankseq
003100990413     C                   SETON                                        99
003200990413     C                   endif
003300010904
003400050211      /free
003401170418          setll *start empmast;
003500170418       // setll *loval emprec;
003600050211          read emprec;
003701170418
003800050222          dow  not(%eof(empmast));
003900020410
004000050528           if %scan(get_Cat(status):'AP') <> *zeros;
004100050211            // Get Division and Bank account if found
004200050211             chain (emp:pbtype) pbfmt;
004300050211
004400050211             setll (pbtype:emp) pbnfmt;
004500050211             reade (pbtype:emp) pbnfmt;
004600051118             if not(%eof(paybanknet))
004700051118                 and  pbnflag = *blanks
004800051118                  or not(%eof(paybanknet))
004900051118                   and  pbnflag = 'Y' and mim_type = *blanks;
005000050222               dow   not(%eof(paybanknet));
005100050211                pbsbank = pbnbank;
005200050211                pbsdivis = pbndivis;
005300050211                write pbsfmt;
005400050211               reade (pbtype:emp) pbnfmt;
005500050211               enddo;
005600050211              else;
005700050211
005800050211              pbsbank = bank;
005900050211              pbsdivis = divis;
006000050211              write pbsfmt;
006100050211             endif;
006200050211           endif;
006300050211           read emprec;
006400050211          enddo;
006500020410
006600050211          *inlr = *on;
006700050211          return;
006800170418
006900990413     **

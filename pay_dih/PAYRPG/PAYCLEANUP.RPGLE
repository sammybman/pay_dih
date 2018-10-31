000100960315     H
000200090415      // PROGRAMMER  S.BRITTON
000300011109     Fpaycon    if   e             disk
000400011109     Fpayeytdl02uf   e           K disk
000500020612     Fpayhisl04 uf   e           k disk
000600020612     Fpaytranl04uf   e           k disk
000700090415      //
000800960506     D                sds
000900960506     D  @pgm             *proc
001000090415      //
001100020612     Dcurdate          s               d   datfmt(*iso)
001200970219     Dstopiso          s               d   datfmt(*iso)
001300011109     Dlimityear        s              4  0
001400020612     Dlimitdate        s              8  0
001500090415      //
001600960315
001601090415      /FREE
001700090415       setll 1 pconfmt;
001800090415       read pconfmt;
001900960315
002000090415       //
002100090415       limityear = *year - pcytd;
002101090415      /END-FREE
002200020612     C                   time                    curdate
002300020612     C     curdate       subdur    pchis:*y      stopiso
002400020612     C                   move      stopiso       limitdate
002500090415      //
002600960315
002700970219
002701090415      /FREE
002800090415       setll *loval ytdrec;
002900090415       read ytdrec;
003000090415       // Process File until EOF of limit condition
003100090415       dow not(%eof) and year <= limityear;
003200090415         //
003300090415         delete ytdrec;
003400090415         //
003500090415         read ytdrec;
003600090415       enddo;
003700970219
003800090415       // History file
003900090415       setll *loval phisfmt;
004000090415       read phisfmt;
004100020612
004200090415       dow not(%eof(payhisl04))
004300090415             and  phispdate < limitdate;
004400090415         //
004500090415         delete phisfmt;
004600090415         //
004700090415         read phisfmt;
004800090415       enddo;
004900090415
005000090415       //
005100090415       // Transaction file
005200090415       setll *loval ptisfmt;
005300090415       read ptisfmt;
005400090415       dow not(%eof(paytranl04))
005500090415             and  ptrnpdate < limitdate;
005600090415         //
005700090415         delete ptisfmt;
005800090415         //
005900090415         read ptisfmt;
006000090415       enddo;
006100020612
006200090415       *inlr = *on;
006300090415       return;
006301090415      /END-FREE

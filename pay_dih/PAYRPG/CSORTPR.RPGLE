000100010510      * Filename:  PAYLIB/PAYRPG(CSORTPR)
000200990909
000300000000      **************************************************************************
000400000000      * SortIt - Uses the qSort function from C
000500000000      *********************************************************************
000600000000
000700000000      * DataStart Holds the _address_ of the Field/Array/MODS to be sorted
000800000000      * Elements  Is the number of elements to be sorted
000900000000      * Size      Contains the length in bytes of each element
001000000000      * CompFunc  A procedure pointer to the user supplied comparison routine
001100000000      *
001200000000      * The user routine has to define it's procedure interface as follows:
001300000000
001400990907      *  D SeqCity         PI            10I 0
001500990907      *  D   Parm1                         *   Value
001600990907      *  D   Parm2                         *   Value
001700000000
001800000000      * RPG code has to compare the two elements passed & return Low/High/Equal
001900000000
002000000000      *
002100000000      * Example of calling Sortit:
002200000000      *
002300990907      *  C                   CallP     SortIt(%Addr(VendorDS):Vendors:
002400990907      *  C                                %Size(VendorDS):
002500990907      *  C                                %PAddr('SEQCITY') )
002600000000
002700000000     D SortIt          PR                  ExtProc('qsort')
002800000000     D  DataStart                      *   Value
002900000000     D  Elements                     10U 0 Value
003000000000     D  Size                         10U 0 Value
003100000000     D  CompFunc                       *   ProcPtr Value
003200000000
003300000000
003400000000      ********************************************************************
003500000000      * FindIt (Uses the bsearch function from C)
003600000000      ********************************************************************
003700000000
003800000000      * Parameters are as for Sortit except for the additional parameter
003900000000      * Lookfor which contains the address of the value to search for.
004000000000      *
004100000000      * bsearch assumes that the area to be sorted is already sequenced
004200000000      * on the key to be searched for.
004300000000      *
004400000000      * Note that the parameters passed to the Compare routine are the same
004500000000      *      as for the sort, but the first parametrer will be the address
004600000000      *      of the original LookFor parameter.
004700000000
004800000000      * Example:
004900000000      *
005000990907      *  C                   Eval      Address = FindIt(%Addr(FindCity):
005100990907      *  C                                       %Addr(VendorDS):
005200990907      *  C                                       Vendors:
005300990907      *  C                                       %Size(VendorDS):
005400990907      *  C                                       %PAddr('SEARCHCITY'))
005500000000
005600000000     D FindIt          PR              *   ExtProc('bsearch')
005700000000     D  LookFor                        *   Value
005800000000     D  DataStart                      *   Value
005900000000     D  Elements                     10U 0 Value
006000000000     D  Size                         10U 0 Value
006100990721     D  CompFunc                       *   ProcPtr Value
006200990721
006300990721
006400990721      * The following constants represent the values to be returned by the
006500990721      *     user written RPG routine to qsort and bsearch
006600990721
006700990721     D High            C                   1
006800990721     D Low             C                   -1
006900990721     D Equal           C                   0
007000990721

#as:
#objdump: -s -t
#name: .byte Assembles bytes in text
#source: cert0050a.s


dump\.o:     file format coff-pic30

SYMBOL TABLE:
\[  0\]\(sec -2\)\(fl 0x00\)\(ty   0\)\(scl 103\) \(nx 1\) 0x00000000 fake
File 
\[  2\]\(sec -1\)\(fl 0x00\)\(ty   0\)\(scl   3\) \(nx 0\) 0x00000001 __C30COFF
\[  3\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x000000ee L11
\[  4\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x000000f4 L21
\[  5\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x000000fc L31
\[  6\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000100 L41
\[  7\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000104 L51
\[  8\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000001c byte1
\[  9\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000001e byte2
\[ 10\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000020 var3
\[ 11\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000022 byte3
\[ 12\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000024 var2
\[ 13\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000002a var1
\[ 14\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000002e byte6
\[ 15\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000030 byte7
\[ 16\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000010a L61
\[ 17\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000110 L71
\[ 18\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000118 L81
\[ 19\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x0000011e L91
\[ 20\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   6\) \(nx 0\) 0x00000122 L1
\[ 21\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   3\) \(nx 1\) 0x00000000 \.text
AUX scnlen 0x24c nreloc 26 nlnno 0
\[ 23\]\(sec  2\)\(fl 0x00\)\(ty   0\)\(scl   3\) \(nx 1\) 0x00000000 \.data
AUX scnlen 0x0 nreloc 0 nlnno 0
\[ 25\]\(sec  3\)\(fl 0x00\)\(ty   0\)\(scl   3\) \(nx 1\) 0x00000000 \.bss
AUX scnlen 0x0 nreloc 0 nlnno 0
\[ 27\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   2\) \(nx 0\) 0x000000ea main
\[ 28\]\(sec  1\)\(fl 0x00\)\(ty   0\)\(scl   2\) \(nx 0\) 0x00000048 foo
\[ 29\]\(sec  0\)\(fl 0x00\)\(ty   0\)\(scl   2\) \(nx 0\) 0x00000000 externdefine
\[ 30\]\(sec  0\)\(fl 0x00\)\(ty   0\)\(scl   2\) \(nx 0\) 0x00000000 main2


Contents of section \.text:
 0000 000004 000000 000004 000000  \.\.\.\.\.\.\.\.\.\.\.\.
 0008 000004 000000 000004 000000  \.\.\.\.\.\.\.\.\.\.\.\.
 0010 000004 000000 000004 000000  \.\.\.\.\.\.\.\.\.\.\.\.
 0018 000004 000000 000000 7f0000  \.\.\.\.\.\.\.\.\.\.\.\.
 0020 7f0000 800000 31d400 000000  \.\.\.\.\.\.1.\.\.\.\.
 0028 ffa500 393000 000000 5a0000  .�\.90\.\.\.\.Z\.\.
 0030 010000 000004 000000 000004  \.\.\.\.\.\.\.\.\.\.\.\.
 0038 000000 000004 000000 000004  \.\.\.\.\.\.\.\.\.\.\.\.
 0040 000000 000004 000000 0000ff  \.\.\.\.\.\.\.\.\.\.\..
 0048 0000ff 546500 737400 206f00  \.\..Te\.st\. o\.
 0050 662000 6c6900 737400 696e00  f \.li\.st\.in\.
 0058 672000 636f00 6e7400 696e00  g \.co\.nt\.in\.
 0060 756100 746900 6f6e00 206c00  ua\.ti\.on\. l\.
 0068 696e00 657300 2e2000 205400  in\.es\.\. \. T\.
 0070 686900 732000 6c6900 6e6500  hi\.s \.li\.ne\.
 0078 207300 686f00 756c00 642000   s\.ho\.ul\.d \.
 0080 626500 207200 656100 6c6c00  be\. r\.ea\.ll\.
 0088 792000 726500 616c00 6c7900  y \.re\.al\.ly\.
 0090 207200 656100 6c6c00 792000   r\.ea\.ll\.y \.
 0098 6c6f00 6e6700 207300 6f2000  lo\.ng\. s\.o \.
 00a0 746800 617400 206700 617300  th\.at\. g\.as\.
 00a8 206900 732000 666f00 726300   i\.s \.fo\.rc\.
 00b0 656400 207400 6f2000 757300  ed\. t\.o \.us\.
 00b8 652000 636f00 6e7400 696e00  e \.co\.nt\.in\.
 00c0 756100 746900 6f6e00 206c00  ua\.ti\.on\. l\.
 00c8 696e00 657300 2e0a00 526500  in\.es\.\.\.\.Re\.
 00d0 737500 6d6500 206c00 697300  su\.me\. l\.is\.
 00d8 746900 6e6700 206900 6e2000  ti\.ng\. i\.n \.
 00e0 6c6900 737400 206600 696c00  li\.st\. f\.il\.
 00e8 652e00 550020 160020 86834a  e\.\.U\. \.\. ..J
 00f0 06acb8 160020 86834a 06acb8  \...\.\. ..J\...
 00f8 160020 86834a 06acb8 160020  \.\. ..J\...\.\. 
 0100 86834a 06acb8 160020 86834a  ..J\...\.\. ..J
 0108 06acb8 160020 86834a 06acb8  \...\.\. ..J\...
 0110 160020 86834a 06acb8 160020  \.\. ..J\...\.\. 
 0118 86834a 06acb8 160020 86834a  ..J\...\.\. ..J
 0120 06acb8 000004 000000         \...\.\.\.\.\.\.   
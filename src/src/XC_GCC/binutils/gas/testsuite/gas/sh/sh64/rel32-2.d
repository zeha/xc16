#as: --abi=32
#objdump: -sr
#source: rel-2.s
#name: MOVI: PC+1-relative relocs, 32-bit ABI.

.*:     file format .*-sh64.*

RELOCATION RECORDS FOR \[\.text\]:
OFFSET  *TYPE  *VALUE 
0+8 R_SH_IMM_LOW16_PCREL  \.data\+0x0+7
0+c R_SH_IMM_LOW16_PCREL  \.data\+0x0+b
0+10 R_SH_IMM_MEDLOW16_PCREL  \.data\+0x0+f
0+1c R_SH_IMM_LOW16_PCREL  \.data\+0x0+27
0+20 R_SH_IMM_LOW16_PCREL  \.data\+0x0+27
0+24 R_SH_IMM_MEDLOW16_PCREL  \.data\+0x0+23
0+30 R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+8
0+34 R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+c
0+38 R_SH_IMM_MEDLOW16_PCREL  \.othertext\+0x0+10
0+44 R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+28
0+48 R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+28
0+4c R_SH_IMM_MEDLOW16_PCREL  \.othertext\+0x0+24
0+58 R_SH_IMM_LOW16_PCREL  extern2\+0xf*ffffffff
0+5c R_SH_IMM_LOW16_PCREL  extern3\+0xf*ffffffff
0+60 R_SH_IMM_MEDLOW16_PCREL  extern4\+0xf*ffffffff
0+6c R_SH_IMM_LOW16_PCREL  extern6\+0x0+f
0+70 R_SH_IMM_LOW16_PCREL  extern7\+0x0+b
0+74 R_SH_IMM_MEDLOW16_PCREL  extern8\+0x0+3
0+80 R_SH_IMM_LOW16_PCREL  gdata2\+0xf*ffffffff
0+84 R_SH_IMM_LOW16_PCREL  gdata3\+0xf*ffffffff
0+88 R_SH_IMM_MEDLOW16_PCREL  gdata4\+0xf*ffffffff
0+94 R_SH_IMM_LOW16_PCREL  gdata6\+0x0+f
0+98 R_SH_IMM_LOW16_PCREL  gdata7\+0x0+b
0+9c R_SH_IMM_MEDLOW16_PCREL  gdata8\+0x0+3
0+a8 R_SH_IMM_LOW16_PCREL  gothertext2\+0xf*ffffffff
0+ac R_SH_IMM_LOW16_PCREL  gothertext3\+0xf*ffffffff
0+b0 R_SH_IMM_MEDLOW16_PCREL  gothertext4\+0xf*ffffffff
0+bc R_SH_IMM_LOW16_PCREL  gothertext6\+0x0+f
0+c0 R_SH_IMM_LOW16_PCREL  gothertext7\+0x0+b
0+c4 R_SH_IMM_MEDLOW16_PCREL  gothertext8\+0x0+3
0+ R_SH_IMM_MEDLOW16_PCREL  \.data\+0x0+3
0+4 R_SH_IMM_LOW16_PCREL  \.data\+0x0+7
0+14 R_SH_IMM_MEDLOW16_PCREL  \.data\+0x0+1b
0+18 R_SH_IMM_LOW16_PCREL  \.data\+0x0+1f
0+28 R_SH_IMM_MEDLOW16_PCREL  \.othertext\+0x0+4
0+2c R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+8
0+3c R_SH_IMM_MEDLOW16_PCREL  \.othertext\+0x0+1c
0+40 R_SH_IMM_LOW16_PCREL  \.othertext\+0x0+20
0+50 R_SH_IMM_MEDLOW16_PCREL  extern1\+0xf*ffffffff
0+54 R_SH_IMM_LOW16_PCREL  extern1\+0x0+3
0+64 R_SH_IMM_MEDLOW16_PCREL  extern5\+0x0+7
0+68 R_SH_IMM_LOW16_PCREL  extern5\+0x0+b
0+78 R_SH_IMM_MEDLOW16_PCREL  gdata1\+0xf*ffffffff
0+7c R_SH_IMM_LOW16_PCREL  gdata1\+0x0+3
0+8c R_SH_IMM_MEDLOW16_PCREL  gdata5\+0x0+7
0+90 R_SH_IMM_LOW16_PCREL  gdata5\+0x0+b
0+a0 R_SH_IMM_MEDLOW16_PCREL  gothertext1\+0xf*ffffffff
0+a4 R_SH_IMM_LOW16_PCREL  gothertext1\+0x0+3
0+b4 R_SH_IMM_MEDLOW16_PCREL  gothertext5\+0x0+7
0+b8 R_SH_IMM_LOW16_PCREL  gothertext5\+0x0+b

Contents of section \.text:
 0000 cc0000a0 c80000a0 cc0000a0 cc0000a0  .*
 0010 cc0000a0 cc0000a0 c80000a0 cc0000a0  .*
 0020 cc0000a0 cc0000a0 cc0000a0 c80000a0  .*
 0030 cc0000a0 cc0000a0 cc0000a0 cc0000a0  .*
 0040 c80000a0 cc0000a0 cc0000a0 cc0000a0  .*
 0050 cc0000a0 c80000a0 cc0000a0 cc0000a0  .*
 0060 cc0000a0 cc0000a0 c80000a0 cc0000a0  .*
 0070 cc0000a0 cc0000a0 cc0000a0 c80000a0  .*
 0080 cc0000a0 cc0000a0 cc0000a0 cc0000a0  .*
 0090 c80000a0 cc0000a0 cc0000a0 cc0000a0  .*
 00a0 cc0000a0 c80000a0 cc0000a0 cc0000a0  .*
 00b0 cc0000a0 cc0000a0 c80000a0 cc0000a0  .*
 00c0 cc0000a0 cc0000a0                    .*
Contents of section \.data:
 0000 00000000 00000000 00000000 00000000  .*
 0010 00000000 00000000 00000000 00000000  .*
 0020 00000000 00000000 00000000 00000000  .*
 0030 00000000 00000000 00000000 00000000  .*
 0040 00000000                             .*
Contents of section \.othertext:
 0000 6ff0fff0 6ff0fff0 6ff0fff0 6ff0fff0  .*
 0010 6ff0fff0 6ff0fff0 6ff0fff0 6ff0fff0  .*
 0020 6ff0fff0 6ff0fff0 6ff0fff0 6ff0fff0  .*
 0030 6ff0fff0 6ff0fff0 6ff0fff0 6ff0fff0  .*
 0040 6ff0fff0                             .*

#objdump: -r
#name: BFD_RELOC_PIC30_PSVOFFSET, psvoffset(local .text) in .data

# This test ensures that the assembler creates a relocation when
# taking the psvoffset of a local .text symbol in an allocation directive
# in the .data section.
# 

.*: .*

RELOCATION RECORDS FOR \[\.data\]\:
OFFSET   TYPE              VALUE 
00000001 PSVOFFSET         test




/* numbers_to_bin_c.h */

#ifndef NUMBERS_TO_BIN_C_
#define NUMBERS_TO_BIN_C_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*  prototypes */

void uchar_2binarray(unsigned char hex, int bin[8]);
void uchar_2bin(unsigned char hex, char bin[8]);
void dec2bin(int dec, char *bin);
int charhex2bin(char *hex, char *bin);

#endif

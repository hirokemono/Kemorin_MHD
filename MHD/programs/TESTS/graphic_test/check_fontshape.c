
/* check_fontshape*/

#include <stdio.h>

#include "numbers_to_bin_c.h"
#include "ysglfontdata.h"

int main(){
	int i, j, iskip, nvart;
	char out[9];
	int iout[8];
	
	
	iskip = 4;
	nvart = 8;
	printf("YsFont8x8 iskip = %d\n",iskip);
	for(i = 0x20; i<0x7f; i++){ 
		printf("ID: %d HEX ID: %x Char: %c\n",i, i, i);
		for (j = 0; j < nvart; j++) {
			uchar_2binarray(YsFont8x8[i][iskip*j], iout);
			printf("Line: %2d Dec: %3d Hex: %2x bin: %d%d%d%d%d%d%d%d, \n",j, 
					(int)YsFont8x8[i][iskip*j], (int)YsFont8x8[i][iskip*j],
					iout[0],iout[1],iout[2],iout[3],iout[4],iout[5],iout[6],iout[7]);
		};
	};
	
	iskip = 4;
	nvart = 12;
	printf("YsFont8x12 iskip = %d\n",iskip);
	for(i = 0x20; i<0x7f; i++){ 
		printf("ID: %d HEX ID: %x Char: %c\n",i, i, i);
		for (j = 0; j < nvart; j++) {
			uchar_2bin(YsFont8x12[i][iskip*j], out);
			printf("Line: %2d Dec: %3d Hex: %2x bin: %s, \n",j, 
					(int)YsFont8x12[i][iskip*j], (int)YsFont8x12[i][iskip*j], out);
		};
	};
	
	
	return;
}

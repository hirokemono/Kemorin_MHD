/*********************************************************************
    calypso_rawfile_io_c.c
    fortran wrapper for binary IO
*********************************************************************/

#include <string.h>
#include "calypso_rawfile_io_c.h"

FILE *fp_b;


void open_wt_rawfile(const char *file_name, int *ierr){
    *ierr = 0;
    if ((fp_b = fopen(file_name, "w")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        *ierr = 1;                    /* terminate with error message */
    }
    return;
}

void open_ad_rawfile(const char *file_name, int *ierr){
    *ierr = 0;
    if ((fp_b = fopen(file_name, "a")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        *ierr = 1;                    /* terminate with error message */
    }
    return;
}

void open_rd_rawfile(const char *file_name, int *ierr){
    *ierr = 0;
    if ((fp_b = fopen(file_name, "r")) == NULL) {
        fprintf(stderr, "Cannot open file!: %s\n", file_name);
        *ierr = 1;                    /* terminate with error message */
    }
    return;
}

void close_rawfile(void){
    fclose(fp_b);
    return;
}

void rawseek_go_fwd(int *ioffset, int *ierr){
    *ierr = fseek(fp_b, *ioffset, SEEK_CUR);
    return;
}

void rawread_32bit(int *iflag_swap, int *ilength, void *buf, int *lenchara){
    *lenchara = (int) fread((char *) buf, sizeof(char), *ilength, fp_b);
    /*
    int i;
    printf("iflag_swap %d\n", *iflag_swap);
    printf("original_32:\n");
    for(i=0;i<*ilength;i++){printf("%x ", (char *) buf)[i]);};
    printf("\n");
    */
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_4(*ilength, (char *) buf);};
    /*
    printf("converted_32:\n");
    for(i=0;i<*ilength;i++){printf("%x ", (char *) buf[i]);};
    printf("\n");
    */
    return;
}

void rawread_64bit(int *iflag_swap, int *ilength, void *buf, int *lenchara){
    *lenchara = (int) fread((char *) buf, sizeof(char), *ilength, fp_b);
    /*
     int i;
    printf("iflag_swap %d\n", *iflag_swap);
    printf("original_64:\n");
    for(i=0;i<*ilength;i++){printf("%x ", (char *) buf[i]);};
    printf("\n");
    */
    if(*iflag_swap == IFLAG_SWAP) {byte_swap_8(*ilength, (char *) buf);};
    /*
    printf("converted_64:\n");
    for(i=0;i<*ilength;i++){printf("%x ", (char *) buf[i]);};
    printf("\n");
    */
    return;
}

void rawwrite(int *ilength, void *buf, int *lenchara){
    *lenchara = (int) fwrite((char *) buf, sizeof(char), *ilength, fp_b);
    return;
}

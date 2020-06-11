
#include <stdlib.h>
#include "skip_comment_c.h"
#include "calypso_zlib_io_c.h"

FILE *fp_y;

void swap_4byte(char *buf8){
	int j;
	char tmpbuf[4];
	
	for(j=0;j<4;j++){tmpbuf[j] = buf8[j];};
	for(j=0;j<4;j++){buf8[j] =   buf8[j+4];};
	for(j=0;j<4;j++){buf8[j+4] = tmpbuf[j];};
	/*
	long tako = 16;
	char *ctako;
	ctako = (char *) &tako;
	printf("ctako ");
	for(i=0;i<8;i++){printf("%d ", ctako[i]);};
	printf("\n");
	*/
	
	return;
}

void swap_fortran_64bit(int *ilength, char *buf){
	int i;
	for(i=0;i<*ilength/8;i++){swap_4byte((char *) &buf[8*i]);}
    return;
}

int main(){
	char UNIX[4] = "UNIX";
	int ierr;
	int i_UNIX = (int) UNIX[0] * 256*256*256
			+ (int) UNIX[1] * 256*256
			+ (int) UNIX[2] * 256
			+ (int) UNIX[3];
	int i_XINU = (int) UNIX[3] * 256*256*256
			+ (int) UNIX[2] * 256*256
			+ (int) UNIX[1] * 256
			+ (int) UNIX[0];
	int iflag_keep = 0;
	int iflag_swap = 0;
	int ilength;
	int lenchara;
	int itmp = 0;
	int itmp_gz = 0;
	char bin_name[255] = "iso_temp2.800001.inb";
	char gzip_name[255] = "iso_temp3.800001.inb.gz";
	char buf[8192];
	
	long nprocs = 0;
	long *itmp1_mp,    *n_inter,    *nele;
	long nnod_gl, nele_gl;
	long nnod_ele, eletype;
	long *ie;
	double *xx;
	
	long nprocs2 = 0;
	long *n_inter2;
	long nnod_gl2 = 0;
	long num_field = 0;
	long ntot_comp = 0;
	long *num_comp;
	char **name;
	double *d_nod;
	
	long nprocs_gz = 0;
	long *itmp1_mp_gz, *n_inter_gz, *nele_gz;
	long nnod_gl_gz, nele_gl_gz;
	long nnod_ele_gz, eletype_gz;
	long *ie_gz;
	double *xx_gz;
	
	long nprocs2_gz = 0;
	long *n_inter2_gz;
	long num_field_gz = 0;
	long ntot_comp_gz = 0;
	long *num_comp_gz;
	char **name_gz;
	double *d_nod_gz;
	
	int i, j;
	
	open_rd_rawfile(bin_name, &ierr);
	ilength = sizeof(int);
	rawread_32bit(&iflag_swap, &ilength, &itmp, &lenchara);
	if(itmp != i_UNIX){iflag_swap = 1;};
	
	ilength = sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, &nprocs, &lenchara);
	itmp1_mp = (long *) calloc(nprocs,sizeof(long));
	n_inter =  (long *) calloc(nprocs,sizeof(long));
	nele =     (long *) calloc(nprocs,sizeof(long));
	
	iflag_swap = 0;
	ilength = nprocs*sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
	rawread_64bit(&iflag_swap, &ilength, n_inter, &lenchara);
	
	nnod_gl = 0;
	for(i=0;i<nprocs;i++){nnod_gl = nnod_gl + n_inter[i];};
	xx = (double *) calloc(3*nnod_gl,sizeof(double));
	
	for(i=0;i<3;i++){
		ilength = nprocs*sizeof(long);
		rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
		ilength = nnod_gl*sizeof(double);
		rawread_64bit(&iflag_swap, &ilength, &xx[nnod_gl*i], &lenchara);
	};
	
	ilength = sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, &nnod_ele, &lenchara);
	rawread_64bit(&iflag_swap, &ilength, &eletype, &lenchara);
	
	ilength = nprocs*sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
	rawread_64bit(&iflag_swap, &ilength, nele, &lenchara);
	
	nele_gl = 0;
	for(i=0;i<nprocs;i++){nele_gl = nele_gl + nele[i];};
	ie = (long *) calloc(nnod_ele*nele_gl,sizeof(long));
	
	for(i=0;i<nnod_ele;i++){
		ilength = nprocs*sizeof(long);
		rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
		ilength = nele_gl*sizeof(long);
		rawread_64bit(&iflag_swap, &ilength, &ie[nele_gl*i], &lenchara);
	};
	
	printf("nprocs %d \n", nprocs);
	printf("n_inter ");
	for(i=0;i<nprocs;i++){printf("%ld ", n_inter[i]);};
	printf("\n");
	printf("nnod_gl %d \n", nnod_gl);
	printf("xx_1 %le %le %le \n", xx[0], xx[nnod_gl  ], xx[2*nnod_gl  ]);
	printf("xx_2 %le %le %le \n", xx[1], xx[nnod_gl+1], xx[2*nnod_gl+1]);
	printf("xx_3 %le %le %le \n", xx[2], xx[nnod_gl+2], xx[2*nnod_gl+2]);
	
	printf("xx_1 %le %le %le \n", xx[nnod_gl-3], xx[2*nnod_gl-3], xx[3*nnod_gl-3]);
	printf("xx_1 %le %le %le \n", xx[nnod_gl-2], xx[2*nnod_gl-2], xx[3*nnod_gl-2]);
	printf("xx_1 %le %le %le \n", xx[nnod_gl-1], xx[2*nnod_gl-1], xx[3*nnod_gl-1]);
	
	printf("nnod_ele %d \n", nnod_ele);
	printf("eletype %d \n", eletype);
	printf("nele_gl %d \n", nele_gl);
	printf("nele ");
	for(i=0;i<nprocs;i++){printf("%ld ", nele[i]);};
	printf("\n");
	printf("ie_1 %d %d %d \n", ie[0], ie[nele_gl  ], ie[2*nele_gl  ]);
	printf("ie_2 %d %d %d \n", ie[1], ie[nele_gl+1], ie[2*nele_gl+1]);
	printf("ie_3 %d %d %d \n", ie[2], ie[nele_gl+2], ie[2*nele_gl+2]);
	
	printf("ie_1 %d %d %d \n", ie[nele_gl-3], ie[2*nele_gl-3], ie[3*nele_gl-3]);
	printf("ie_2 %d %d %d \n", ie[nele_gl-2], ie[2*nele_gl-2], ie[3*nele_gl-2]);
	printf("ie_3 %d %d %d \n", ie[nele_gl-1], ie[2*nele_gl-1], ie[3*nele_gl-1]);
	
	
	ilength = sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, &nprocs2, &lenchara);
	n_inter2 =  (long *) calloc(nprocs2,sizeof(long));
	
	ilength = nprocs2*sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
	rawread_64bit(&iflag_swap, &ilength, n_inter2, &lenchara);
	
	ilength = sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, &num_field, &lenchara);
	
	num_comp =  (long *) calloc(num_field,sizeof(long));
	name =     (char **) malloc(num_field*sizeof(char *));
	for(i=0;i<num_field;i++){name[i] = (char *) calloc(KCHARA_C,sizeof(char));};
	
	ilength = num_field*sizeof(long);
	rawread_64bit(&iflag_swap, &ilength, num_comp, &lenchara);
	for(i=0;i<num_field;i++){
		ilength = (KCHARA_C-1)*sizeof(char);
		rawread_64bit(&iflag_keep, &ilength, name[i], &lenchara);
		name[i] = trim(name[i]);
	}
	ntot_comp = 0;
	for(i=0;i<num_field;i++){ntot_comp = ntot_comp + num_comp[i];};
	nnod_gl2 = 0;
	for(i=0;i<nprocs2;i++){nnod_gl2 = nnod_gl2 + n_inter2[i];};
	d_nod = (double *) calloc(ntot_comp*nnod_gl2,sizeof(double));
	
	for(i=0;i<ntot_comp;i++){
		ilength = nprocs2*sizeof(long);
		rawread_64bit(&iflag_swap, &ilength, itmp1_mp, &lenchara);
		ilength = nnod_gl2*sizeof(double);
		rawread_64bit(&iflag_swap, &ilength, &d_nod[nnod_gl2*i], &lenchara);
	};
	
	
	printf("nprocs2 %d \n", nprocs2);
	printf("n_inter2 ");
	for(i=0;i<nprocs2;i++){printf("%ld ", n_inter2[i]);};
	printf("\n");
	printf("nnod_gl2 %d \n", nnod_gl2);
	printf("num_field %d \n", num_field);
	printf("num_comp ");
	for(i=0;i<num_field;i++){printf("%ld ", num_comp[i]);};
	printf("\n");
	printf("name ");
	for(i=0;i<num_field;i++){printf("%d %s \n", i, name[i]);};
	printf("\n");
	printf("d_nod_1 %le \n", d_nod[0]);
	printf("d_nod_2 %le \n", d_nod[1]);
	printf("d_nod_3 %le \n", d_nod[2]);
	
	printf("d_nod_3 %le \n", d_nod[nnod_gl2-3]);
	printf("d_nod_2 %le \n", d_nod[nnod_gl2-2]);
	printf("d_nod_1 %le \n", d_nod[nnod_gl2-1]);
	
	close_rawfile();
	
	open_rd_gzfile(gzip_name);
	ilength = sizeof(int);
	gzread_32bit_f(&iflag_swap, &ilength, (char *) &itmp_gz, &ierr);
	if(itmp_gz != i_UNIX){iflag_swap = 1;};
	
	ilength = sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &nprocs_gz, &ierr);
	itmp1_mp_gz = (long *)calloc(nprocs_gz,sizeof(long));
	n_inter_gz =  (long *)calloc(nprocs_gz,sizeof(long));
	nele_gz =     (long *)calloc(nprocs_gz,sizeof(long));
	
	ilength = nprocs_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) n_inter_gz, &ierr);
	
	nnod_gl_gz = 0;
	for(i=0;i<nprocs_gz;i++){nnod_gl_gz = nnod_gl_gz + n_inter_gz[i];};
	xx_gz = (double *) calloc(3*nnod_gl_gz,sizeof(double));
	
	ilength = nprocs_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
	ilength = nnod_gl_gz*sizeof(double);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &xx_gz[0], &ierr);
	
	ilength = nprocs_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
	ilength = nnod_gl_gz*sizeof(double);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &xx_gz[nnod_gl_gz], &ierr);
	
	ilength = nprocs_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
	ilength = nnod_gl_gz*sizeof(double);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &xx_gz[2*nnod_gl_gz], &ierr);
	
	ilength = sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &nnod_ele_gz, &ierr);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &eletype_gz, &ierr);
	
	ilength = nprocs_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) nele_gz, &ierr);
	
	nele_gl_gz = 0;
	for(i=0;i<nprocs_gz;i++){nele_gl_gz = nele_gl_gz + nele_gz[i];};
	ie_gz = (long *) calloc(nnod_ele_gz*nele_gl_gz,sizeof(long));
	
	for(i=0;i<nnod_ele_gz;i++){
		ilength = nprocs_gz*sizeof(long);
		gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp_gz, &ierr);
		ilength = nele_gl_gz*sizeof(long);
		gzread_64bit_f(&iflag_swap, &ilength, (char *) &ie_gz[nele_gl_gz*i], &ierr);
	};
	
	printf("nprocs_gz %d \n", nprocs_gz);
	printf("n_inter_gz ");
	for(i=0;i<nprocs_gz;i++){printf("%ld ", n_inter_gz[i]);};
	printf("\n");
	printf("nnod_gl_gz %d \n", nnod_gl_gz);
	
	printf("xx_gz_1 %le %le %le \n", xx_gz[0], xx_gz[nnod_gl_gz  ], xx_gz[2*nnod_gl_gz  ]);
	printf("xx_gz_2 %le %le %le \n", xx_gz[1], xx_gz[nnod_gl_gz+1], xx_gz[2*nnod_gl_gz+1]);
	printf("xx_gz_3 %le %le %le \n", xx_gz[2], xx_gz[nnod_gl_gz+2], xx_gz[2*nnod_gl_gz+2]);
	
	printf("xx_gz_1 %le %le %le \n", xx_gz[nnod_gl_gz-3], xx_gz[2*nnod_gl_gz-3], xx_gz[3*nnod_gl_gz-3]);
	printf("xx_gz_1 %le %le %le \n", xx_gz[nnod_gl_gz-2], xx_gz[2*nnod_gl_gz-2], xx_gz[3*nnod_gl_gz-2]);
	printf("xx_gz_1 %le %le %le \n", xx_gz[nnod_gl_gz-1], xx_gz[2*nnod_gl_gz-1], xx_gz[3*nnod_gl_gz-1]);
	
	printf("nnod_ele_gz %d \n", nnod_ele_gz);
	printf("eletype_gz %d \n", eletype_gz);
	printf("nele_gl_gz %d \n", nele_gl_gz);
	printf("nele_gz ");
	for(i=0;i<nprocs_gz;i++){printf("%ld ", nele_gz[i]);};
	printf("\n");
	
	printf("ie_gz_1 %d %d %d \n", ie_gz[0], ie_gz[nele_gl_gz  ], ie_gz[2*nele_gl_gz  ]);
	printf("ie_gz_2 %d %d %d \n", ie_gz[1], ie_gz[nele_gl_gz+1], ie_gz[2*nele_gl_gz+1]);
	printf("ie_gz_3 %d %d %d \n", ie_gz[2], ie_gz[nele_gl_gz+2], ie_gz[2*nele_gl_gz+2]);
	
	printf("ie_gz_1 %d %d %d \n", ie_gz[nele_gl_gz-3], ie_gz[2*nele_gl_gz-3], ie_gz[3*nele_gl_gz-3]);
	printf("ie_gz_2 %d %d %d \n", ie_gz[nele_gl_gz-2], ie_gz[2*nele_gl_gz-2], ie_gz[3*nele_gl_gz-2]);
	printf("ie_gz_3 %d %d %d \n", ie_gz[nele_gl_gz-1], ie_gz[2*nele_gl_gz-1], ie_gz[3*nele_gl_gz-1]);
	
	
	
	ilength = sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &nprocs2_gz, &lenchara);
	n_inter2_gz =  (long *) calloc(nprocs2_gz,sizeof(long));
	
	ilength = nprocs2_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp, &lenchara);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) n_inter2_gz, &lenchara);
	
	ilength = sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) &num_field_gz, &lenchara);
	
	num_comp_gz =  (long *) calloc(num_field_gz,sizeof(long));
	name_gz =     (char **) malloc(num_field_gz*sizeof(char *));
	for(i=0;i<num_field_gz;i++){name_gz[i] = (char *) calloc(KCHARA_C,sizeof(char));};
	
	ilength = num_field_gz*sizeof(long);
	gzread_64bit_f(&iflag_swap, &ilength, (char *) num_comp_gz, &lenchara);
	for(i=0;i<num_field_gz;i++){
		ilength = (KCHARA_C-1)*sizeof(char);
		gzread_64bit_f(&iflag_keep, &ilength, (char *) name_gz[i], &lenchara);
		name_gz[i] = trim(name_gz[i]);
	}
	ntot_comp_gz = 0;
	for(i=0;i<num_field_gz;i++){ntot_comp_gz = ntot_comp_gz + num_comp_gz[i];};
	nnod_gl2 = 0;
	for(i=0;i<nprocs2_gz;i++){nnod_gl2 = nnod_gl2 + n_inter2_gz[i];};
	d_nod_gz = (double *) calloc(ntot_comp_gz*nnod_gl2,sizeof(double));
	
	for(i=0;i<ntot_comp_gz;i++){
		ilength = nprocs2_gz*sizeof(long);
		gzread_64bit_f(&iflag_swap, &ilength, (char *) itmp1_mp, &lenchara);
		ilength = nnod_gl2*sizeof(double);
		gzread_64bit_f(&iflag_swap, &ilength, (char *) &d_nod_gz[nnod_gl2*i], &lenchara);
	};
	
	
	printf("nprocs2_gz %d \n", nprocs2_gz);
	printf("n_inter2_gz ");
	for(i=0;i<nprocs2_gz;i++){printf("%ld ", n_inter2_gz[i]);};
	printf("\n");
	printf("nnod_gl2 %d \n", nnod_gl2);
	printf("num_field_gz %d \n", num_field_gz);
	printf("num_comp_gz ");
	for(i=0;i<num_field_gz;i++){printf("%ld ", num_comp_gz[i]);};
	printf("\n");
	printf("name_gz ");
	for(i=0;i<num_field_gz;i++){printf("%d %s \n", i, name_gz[i]);};
	printf("\n");
	printf("d_nod_gz_1 %le \n", d_nod_gz[0]);
	printf("d_nod_gz_2 %le \n", d_nod_gz[1]);
	printf("d_nod_gz_3 %le \n", d_nod_gz[2]);
	
	printf("d_nod_gz_3 %le \n", d_nod_gz[nnod_gl2-3]);
	printf("d_nod_gz_2 %le \n", d_nod_gz[nnod_gl2-2]);
	printf("d_nod_gz_1 %le \n", d_nod_gz[nnod_gl2-1]);
	
	close_gzfile();
	
	printf("itmp1_mp_gz ");
	for(i=0;i<nprocs_gz;i++){printf("%ld ", itmp1_mp_gz[i]);};
	printf("\n");
	
	
	printf("Error xx \n");
	for(j=0;j<3;j++){
		for(i=0;i<nnod_gl;i++){
			if(xx[i+j*nnod_gl] != xx_gz[i+j*nnod_gl]){
				printf("%d %d %le %le\n", j, i, xx[i+j*nnod_gl], xx_gz[i+j*nnod_gl]);
			};
		};
	};
	
	printf("Error ie \n");
	for(j=0;j<nnod_ele;j++){
		for(i=0;i<nele_gl;i++){
			if(xx[i+j*nele_gl] != xx_gz[i+j*nele_gl]){
				printf("%d %d %d %d\n", j, i, ie[i+j*nele_gl], ie_gz[i+j*nele_gl]);
			};
		};
	};
	
	printf("Error d_nod \n");
	for(j=0;j<ntot_comp;j++){
		for(i=0;i<nnod_gl2;i++){
			if(d_nod[i+j*nnod_gl2] != d_nod_gz[i+j*nnod_gl2]){
				printf("%d %d %le %le\n", j, i, d_nod[i+j*nnod_gl2], d_nod_gz[i+j*nnod_gl2]);
			};
		};
	};
	printf("\n");
}
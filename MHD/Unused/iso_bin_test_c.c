
#include <stdlib.h>
#include "skip_comment_c.h"
#include "calypso_zlib_io_c.h"
#include "m_psf_data_4_viewer_c.h"

struct psf_bin_work{
	int ierr;
	int i_UNIX;
	int i_XINU;
	
	int iflag_keep;
	int iflag_swap;
	int ilength;
	int lchar_out;
	long nprocs;
	
	long *itmp_mp;
};

struct psf_bin_work * init_psf_bin_work(){
	char UNIX[4] = "UNIX";
	struct psf_bin_work *psf_b_WK;
    if ((psf_b_WK = (struct psf_bin_work *) malloc(sizeof(struct psf_bin_work))) == NULL) {
        printf("malloc error fot psf_bin_work \n");
        exit(0);
	}
	psf_b_WK->i_UNIX = (int) UNIX[0] * 256*256*256
			+ (int) UNIX[1] * 256*256
			+ (int) UNIX[2] * 256
			+ (int) UNIX[3];
	psf_b_WK->i_XINU = (int) UNIX[3] * 256*256*256
			+ (int) UNIX[2] * 256*256
			+ (int) UNIX[1] * 256
			+ (int) UNIX[0];
	
	psf_b_WK->iflag_keep = 0;
	psf_b_WK->iflag_swap = 0;
	psf_b_WK->nprocs = 0;
	psf_b_WK->ilength = 0;
	psf_b_WK->lchar_out = 0;
	psf_b_WK->ierr = 0;
	return psf_b_WK;
};

void rawread_64bit_psfchara(struct psf_bin_work *psf_b_WK, void *buf){
	rawread_64bit(&psf_b_WK->iflag_keep, &psf_b_WK->ilength, 
				   buf, &psf_b_WK->lchar_out);
	return;
}
void rawread_64bit_psf(struct psf_bin_work *psf_b_WK, void *buf){
	rawread_64bit(&psf_b_WK->iflag_swap, &psf_b_WK->ilength, 
				   buf, &psf_b_WK->lchar_out);
	return;
}
void gzread_64bit_psfchara(struct psf_bin_work *psf_z_WK, char *textbuf){
	gzread_64bit_f(&psf_z_WK->iflag_keep, &psf_z_WK->ilength, 
				   textbuf, &psf_z_WK->ierr);
	return;
}
void gzread_64bit_psf(struct psf_bin_work *psf_z_WK, char *textbuf){
	gzread_64bit_f(&psf_z_WK->iflag_swap, &psf_z_WK->ilength, 
				   textbuf, &psf_z_WK->ierr);
	return;
}

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
	int itmp = 0;
	int itmp_gz = 0;
	char bin_name[255] = "iso_temp2.800001.inb";
	char gzip_name[255] = "iso_temp3.800001.inb.gz";
	
	long *n_inter,    *nele;
	long eletype;
	double *xx;
	
	long *n_inter2;
	long *num_comp;
	char **name;
	
	long *n_inter_gz, *nele_gz;
	long eletype_gz;
	double *xx_gz;
	
	long *n_inter2_gz;
	long *num_comp_gz;
	char **name_gz;
	
	struct psf_data *psf_b = (struct psf_data *) malloc(sizeof(struct psf_data));;
	struct psf_data *psf_z = (struct psf_data *) malloc(sizeof(struct psf_data));;
	
	struct psf_bin_work *psf_b_WK = init_psf_bin_work();
	struct psf_bin_work *psf_z_WK = init_psf_bin_work();
	
	int i, j;
	
	open_rd_rawfile(bin_name, &psf_b_WK->ierr);
	psf_b_WK->ilength = sizeof(int);
	rawread_32bit(&psf_b_WK->iflag_keep, &psf_b_WK->ilength,
				  &itmp, &psf_b_WK->lchar_out);
	if(itmp != psf_b_WK->i_UNIX){psf_b_WK->iflag_swap = 1;};
	
	psf_b_WK->ilength = sizeof(long);
	rawread_64bit_psf(psf_b_WK, &psf_b_WK->nprocs);
	psf_b_WK->itmp_mp = (long *) calloc(psf_b_WK->nprocs,sizeof(long));
	
	n_inter =  (long *) calloc(psf_b_WK->nprocs,sizeof(long));
	nele =     (long *) calloc(psf_b_WK->nprocs,sizeof(long));
	
	psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
	rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
	rawread_64bit_psf(psf_b_WK, n_inter);
	
	psf_b->nnod_viz = 0;
	for(i=0;i<psf_b_WK->nprocs;i++){psf_b->nnod_viz = psf_b->nnod_viz + n_inter[i];};
	
	alloc_viz_node_s(psf_b);
	xx = (double *) calloc(3*psf_b->nnod_viz,sizeof(double));
	
	for(i=0;i<3;i++){
		psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
		rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
		psf_b_WK->ilength = psf_b->nnod_viz*sizeof(double);
		rawread_64bit_psf(psf_b_WK, &xx[psf_b->nnod_viz*i]);
	};
	
	psf_b_WK->ilength = sizeof(long);
	rawread_64bit_psf(psf_b_WK, &psf_b->nnod_4_ele_viz);
	rawread_64bit_psf(psf_b_WK, &eletype);
	
	psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
	rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
	rawread_64bit_psf(psf_b_WK, nele);
	
	psf_b->nele_viz = 0;
	for(i=0;i<psf_b_WK->nprocs;i++){psf_b->nele_viz = psf_b->nele_viz + nele[i];};
	
	alloc_viz_ele_s(psf_b);
	long *ie = (long *) calloc(psf_b->nele_viz, sizeof(long));
	
	for(j=0;j<psf_b->nnod_4_ele_viz;j++){
		psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
		rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
		psf_b_WK->ilength = psf_b->nele_viz*sizeof(long);
		rawread_64bit_psf(psf_b_WK, ie);
		for(i=0;i<psf_b->nele_viz;i++){
			psf_b->ie_viz[i][j] = ie[i];
		};
	};
	
	printf("psf_b_WK->nprocs %d \n", psf_b_WK->nprocs);
	printf("n_inter ");
	for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", n_inter[i]);};
	printf("\n");
	printf("psf_b->nnod_viz %d \n", psf_b->nnod_viz);
	printf("xx_1 %le %le %le \n", xx[0], xx[psf_b->nnod_viz  ], xx[2*psf_b->nnod_viz  ]);
	printf("xx_2 %le %le %le \n", xx[1], xx[psf_b->nnod_viz+1], xx[2*psf_b->nnod_viz+1]);
	printf("xx_3 %le %le %le \n", xx[2], xx[psf_b->nnod_viz+2], xx[2*psf_b->nnod_viz+2]);
	
	printf("xx_1 %le %le %le \n", xx[psf_b->nnod_viz-3], xx[2*psf_b->nnod_viz-3], xx[3*psf_b->nnod_viz-3]);
	printf("xx_1 %le %le %le \n", xx[psf_b->nnod_viz-2], xx[2*psf_b->nnod_viz-2], xx[3*psf_b->nnod_viz-2]);
	printf("xx_1 %le %le %le \n", xx[psf_b->nnod_viz-1], xx[2*psf_b->nnod_viz-1], xx[3*psf_b->nnod_viz-1]);
	
	printf("psf_b->nnod_4_ele_viz %d \n", psf_b->nnod_4_ele_viz);
	printf("eletype %d \n", eletype);
	printf("psf_b->nele_viz %d \n", psf_b->nele_viz);
	printf("nele ");
	for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", nele[i]);};
	printf("\n");
	printf("ie_1 %d %d %d \n", psf_b->ie_viz[0][0], psf_b->ie_viz[0][1], psf_b->ie_viz[0][2]);
	printf("ie_2 %d %d %d \n", psf_b->ie_viz[1][0], psf_b->ie_viz[1][1], psf_b->ie_viz[1][2]);
	printf("ie_3 %d %d %d \n", psf_b->ie_viz[2][0], psf_b->ie_viz[2][1], psf_b->ie_viz[2][2]);
	
	printf("ie_3 %d %d %d \n", psf_b->ie_viz[psf_b->nele_viz-3][0], 
		   psf_b->ie_viz[psf_b->nele_viz-3][1], psf_b->ie_viz[psf_b->nele_viz-3][2]);
	printf("ie_2 %d %d %d \n", psf_b->ie_viz[psf_b->nele_viz-2][0], 
		   psf_b->ie_viz[psf_b->nele_viz-2][1], psf_b->ie_viz[psf_b->nele_viz-2][2]);
	printf("ie_1 %d %d %d \n", psf_b->ie_viz[psf_b->nele_viz-1][0], 
		   psf_b->ie_viz[psf_b->nele_viz-1][1], psf_b->ie_viz[psf_b->nele_viz-1][2]);
	
	
	long nprocs2 = 0;
	psf_b_WK->ilength = sizeof(long);
	rawread_64bit_psf(psf_b_WK, &nprocs2);
	if(psf_b_WK->nprocs != nprocs2){
		printf("Number of processes is wrong!\n");
	};
	
	n_inter2 =  (long *) calloc(psf_b_WK->nprocs,sizeof(long));
	psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
	rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
	rawread_64bit_psf(psf_b_WK, n_inter2);
	
	psf_b_WK->ilength = sizeof(long);
	rawread_64bit_psf(psf_b_WK, &psf_b->nfield);
	
	alloc_psf_field_name_c(psf_b);
	num_comp =  (long *) calloc(psf_b->nfield,sizeof(long));
	name =     (char **) malloc(psf_b->nfield*sizeof(char *));
	for(i=0;i<psf_b->nfield;i++){name[i] = (char *) calloc(KCHARA_C,sizeof(char));};
	
	psf_b_WK->ilength = psf_b->nfield*sizeof(long);
	rawread_64bit_psf(psf_b_WK, num_comp);
	for(i=0;i<psf_b->nfield;i++){
		psf_b_WK->ilength = (KCHARA_C-1)*sizeof(char);
		rawread_64bit_psfchara(psf_b_WK, name[i]);
		name[i] = trim(name[i]);
	}
	psf_b->ncomptot = 0;
	for(i=0;i<psf_b->nfield;i++){psf_b->ncomptot = psf_b->ncomptot + num_comp[i];};
	
	long nnod_tmp = 0;
	for(i=0;i<psf_b_WK->nprocs;i++){nnod_tmp = nnod_tmp + n_inter2[i];};
	if(psf_b->nnod_viz != nnod_tmp){
		printf("Number of node is wrong!\n");
	};
	
	alloc_psf_field_data_c(psf_b);
	double *d_nod = (double *) calloc(psf_b->nnod_viz,sizeof(double));
	
	for(j=0;j<psf_b->ncomptot;j++){
		psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
		rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
		psf_b_WK->ilength = psf_b->nnod_viz * sizeof(double);
		rawread_64bit_psf(psf_b_WK, &d_nod[0]);
		for(i=0;i<psf_b->nnod_viz;i++){
			psf_b->d_nod[i][j] = d_nod[i];
		};
	};
	
	
	printf("n_inter2 ");
	for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", n_inter2[i]);};
	printf("\n");
	printf("psf_b->nfield %d \n", psf_b->nfield);
	printf("num_comp ");
	for(i=0;i<psf_b->nfield;i++){printf("%ld ", num_comp[i]);};
	printf("\n");
	printf("name ");
	for(i=0;i<psf_b->nfield;i++){printf("%d %s \n", i, name[i]);};
	printf("\n");
	printf("d_nod_1 %le \n", psf_b->d_nod[0][0]);
	printf("d_nod_2 %le \n", psf_b->d_nod[1][0]);
	printf("d_nod_3 %le \n", psf_b->d_nod[2][0]);
	
	printf("d_nod_3 %le \n", psf_b->d_nod[psf_b->nnod_viz-3][0]);
	printf("d_nod_2 %le \n", psf_b->d_nod[psf_b->nnod_viz-2][0]);
	printf("d_nod_1 %le \n", psf_b->d_nod[psf_b->nnod_viz-1][0]);
	
	close_rawfile();
	
	open_rd_gzfile(gzip_name);
	psf_z_WK->ilength = sizeof(int);
	gzread_32bit_f(&psf_z_WK->iflag_keep, &psf_z_WK->ilength, 
				   (char *) &itmp_gz, &psf_z_WK->ierr);
	if(itmp_gz != psf_z_WK->i_UNIX){psf_z_WK->iflag_swap = 1;};
	
	psf_z_WK->ilength = sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) &psf_z_WK->nprocs);
	psf_z_WK->itmp_mp = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
	
	n_inter_gz =  (long *)calloc(psf_z_WK->nprocs,sizeof(long));
	nele_gz =     (long *)calloc(psf_z_WK->nprocs,sizeof(long));
	
	psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
	gzread_64bit_psf(psf_z_WK, (char *) n_inter_gz);
	
	psf_z->nnod_viz = 0;
	for(i=0;i<psf_z_WK->nprocs;i++){psf_z->nnod_viz = psf_z->nnod_viz + n_inter_gz[i];};
	
	alloc_viz_node_s(psf_z);
	xx_gz = (double *) calloc(3*psf_z->nnod_viz,sizeof(double));
	
	for(i=0;i<3;i++){
		psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
		gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
		psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
		gzread_64bit_psf(psf_z_WK, (char *) &xx_gz[i*psf_z->nnod_viz]);
	};
	
	psf_z_WK->ilength = sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) &psf_z->nnod_4_ele_viz);
	gzread_64bit_psf(psf_z_WK, (char *) &eletype_gz);
	
	psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
	gzread_64bit_psf(psf_z_WK, (char *) nele_gz);
	
	psf_z->nele_viz = 0;
	for(i=0;i<psf_z_WK->nprocs;i++){psf_z->nele_viz = psf_z->nele_viz + nele_gz[i];};
	
	alloc_viz_ele_s(psf_z);
	long *ie_gz = (long *) calloc(psf_z->nele_viz,sizeof(long));
	
	for(j=0;j<psf_z->nnod_4_ele_viz;j++){
		psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
		gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
		psf_z_WK->ilength = psf_z->nele_viz*sizeof(long);
		gzread_64bit_psf(psf_z_WK, (char *) ie_gz);
		for(i=0;i<psf_z->nele_viz;i++){
			psf_z->ie_viz[i][j] = ie_gz[i];
		};
	};
	free(ie_gz);
	
	printf("psf_z_WK->nprocs %d \n", psf_z_WK->nprocs);
	printf("n_inter_gz ");
	for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", n_inter_gz[i]);};
	printf("\n");
	printf("psf_z->nnod_viz %d \n", psf_z->nnod_viz);
	
	printf("xx_gz_1 %le %le %le \n", xx_gz[0], xx_gz[psf_z->nnod_viz  ], xx_gz[2*psf_z->nnod_viz  ]);
	printf("xx_gz_2 %le %le %le \n", xx_gz[1], xx_gz[psf_z->nnod_viz+1], xx_gz[2*psf_z->nnod_viz+1]);
	printf("xx_gz_3 %le %le %le \n", xx_gz[2], xx_gz[psf_z->nnod_viz+2], xx_gz[2*psf_z->nnod_viz+2]);
	
	printf("xx_gz_1 %le %le %le \n", xx_gz[psf_z->nnod_viz-3], xx_gz[2*psf_z->nnod_viz-3], xx_gz[3*psf_z->nnod_viz-3]);
	printf("xx_gz_1 %le %le %le \n", xx_gz[psf_z->nnod_viz-2], xx_gz[2*psf_z->nnod_viz-2], xx_gz[3*psf_z->nnod_viz-2]);
	printf("xx_gz_1 %le %le %le \n", xx_gz[psf_z->nnod_viz-1], xx_gz[2*psf_z->nnod_viz-1], xx_gz[3*psf_z->nnod_viz-1]);
	
	printf("psf_z->nnod_4_ele_viz %d \n", psf_z->nnod_4_ele_viz);
	printf("eletype_gz %d \n", eletype_gz);
	printf("psf_z->nele_viz %d \n", psf_z->nele_viz);
	printf("nele_gz ");
	for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", nele_gz[i]);};
	printf("\n");
	
	printf("ie_gz_1 %d %d %d \n", psf_z->ie_viz[0][0], psf_z->ie_viz[0][1], psf_z->ie_viz[0][2]);
	printf("ie_gz_2 %d %d %d \n", psf_z->ie_viz[1][0], psf_z->ie_viz[1][1], psf_z->ie_viz[1][2]);
	printf("ie_gz_3 %d %d %d \n", psf_z->ie_viz[2][0], psf_z->ie_viz[2][1], psf_z->ie_viz[2][2]);
	
	printf("ie_gz_3 %d %d %d \n", psf_z->ie_viz[psf_z->nele_viz-3][0],
		   psf_z->ie_viz[psf_z->nele_viz-3][1], psf_z->ie_viz[psf_z->nele_viz-3][2]);
	printf("ie_gz_2 %d %d %d \n", psf_z->ie_viz[psf_z->nele_viz-2][0],
		   psf_z->ie_viz[psf_z->nele_viz-2][1], psf_z->ie_viz[psf_z->nele_viz-2][2]);
	printf("ie_gz_1 %d %d %d \n", psf_z->ie_viz[psf_z->nele_viz-1][0],
		   psf_z->ie_viz[psf_z->nele_viz-1][1], psf_z->ie_viz[psf_z->nele_viz-1][2]);
	
	
	
	long nprocs2_gz = 0;
	psf_z_WK->ilength = sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) &nprocs2_gz);
	if(psf_z_WK->nprocs != nprocs2_gz){
		printf("Number of processes is wrong!\n");
	};
	
	n_inter2_gz =  (long *) calloc(psf_z_WK->nprocs,sizeof(long));
	
	psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) psf_b_WK->itmp_mp);
	gzread_64bit_psf(psf_z_WK, (char *) n_inter2_gz);
	
	psf_z_WK->ilength = sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) &psf_z->nfield);
	
	alloc_psf_field_name_c(psf_z);
	num_comp_gz =  (long *) calloc(psf_z->nfield,sizeof(long));
	name_gz =     (char **) malloc(psf_z->nfield*sizeof(char *));
	for(i=0;i<psf_z->nfield;i++){name_gz[i] = (char *) calloc(KCHARA_C,sizeof(char));};
	
	psf_z_WK->ilength = psf_z->nfield*sizeof(long);
	gzread_64bit_psf(psf_z_WK, (char *) num_comp_gz);
	for(i=0;i<psf_z->nfield;i++){
		psf_z_WK->ilength = (KCHARA_C-1)*sizeof(char);
		gzread_64bit_psfchara(psf_z_WK, (char *) name_gz[i]);
		name_gz[i] = trim(name_gz[i]);
	}
	psf_z->ncomptot = 0;
	for(i=0;i<psf_z->nfield;i++){psf_z->ncomptot = psf_z->ncomptot + num_comp_gz[i];};
	
	long nnod_tmp_gz = 0;
	for(i=0;i<psf_z_WK->nprocs;i++){nnod_tmp_gz = nnod_tmp_gz + n_inter2_gz[i];};
	if(psf_z->nnod_viz != nnod_tmp_gz){
		printf("Number of node is wrong!\n");
	};
	
	alloc_psf_field_data_c(psf_z);
	double *d_nod_gz = (double *) calloc(psf_z->nnod_viz,sizeof(double));
	
	for(j=0;j<psf_z->ncomptot;j++){
		psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
		gzread_64bit_psf(psf_z_WK, (char *) psf_b_WK->itmp_mp);
		psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
		gzread_64bit_psf(psf_z_WK, (char *) d_nod_gz);
		for(i=0;i<psf_z->nnod_viz;i++){
			psf_z->d_nod[i][j] = d_nod_gz[i];
		};
	};
	free(d_nod_gz);
	
	printf("n_inter2_gz ");
	for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", n_inter2_gz[i]);};
	printf("\n");
	printf("psf_z->nfield %d \n", psf_z->nfield);
	printf("num_comp_gz ");
	for(i=0;i<psf_z->nfield;i++){printf("%ld ", num_comp_gz[i]);};
	printf("\n");
	printf("name_gz ");
	for(i=0;i<psf_z->nfield;i++){printf("%d %s \n", i, name_gz[i]);};
	printf("\n");
	printf("d_nod_gz_1 %le \n", psf_z->d_nod[0][0]);
	printf("d_nod_gz_2 %le \n", psf_z->d_nod[1][0]);
	printf("d_nod_gz_3 %le \n", psf_z->d_nod[2][0]);
	
	printf("d_nod_gz_3 %le \n", psf_z->d_nod[psf_z->nnod_viz-3][0]);
	printf("d_nod_gz_2 %le \n", psf_z->d_nod[psf_z->nnod_viz-2][0]);
	printf("d_nod_gz_1 %le \n", psf_z->d_nod[psf_z->nnod_viz-1][0]);
	
	close_gzfile();
	
	printf("psf_z_WK->itmp_mp ");
	for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", psf_z_WK->itmp_mp[i]);};
	printf("\n");
	
	
	printf("Error xx \n");
	for(j=0;j<3;j++){
		for(i=0;i<psf_b->nnod_viz;i++){
			if(xx[i+j*psf_b->nnod_viz] != xx_gz[i+j*psf_b->nnod_viz]){
				printf("%d %d %le %le\n", j, i, xx[i+j*psf_b->nnod_viz], xx_gz[i+j*psf_b->nnod_viz]);
			};
		};
	};
	
	printf("Error ie_viz \n");
	for(j=0;j<psf_b->nnod_4_ele_viz;j++){
		for(i=0;i<psf_b->nele_viz;i++){
			if(psf_b->ie_viz[i][j] != psf_z->ie_viz[i][j]){
				printf("%d %d %d %d\n", j, i, psf_b->ie_viz[i][j], psf_z->ie_viz[i][j]);
			};
		};
	};
	
	printf("Error d_nod \n");
	for(j=0;j<psf_b->ncomptot;j++){
		for(i=0;i<psf_b->nnod_viz;i++){
			if(psf_z->d_nod[i][j] != psf_z->d_nod[i][j]){
				printf("%d %d %le %le\n", j, i, psf_z->d_nod[i][j], psf_z->d_nod[i][j]);
			};
		};
	};
	printf("\n");
}

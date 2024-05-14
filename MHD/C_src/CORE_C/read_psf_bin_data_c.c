/*
//  read_psf_bin_data_c.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2020/06/12.
*/

#include "read_psf_bin_data_c.h"

struct psf_bin_work * init_psf_bin_work(void){
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

void dealloc_psf_bin_work(struct psf_bin_work *psf_b_WK){
    free(psf_b_WK->itmp_mp);
    free(psf_b_WK);
    return;
};

static void rawread_64bit_psf(struct psf_bin_work *psf_b_WK, void *buf){
    psf_b_WK->lchar_out = rawread_64bit(psf_b_WK->iflag_swap,
                                        psf_b_WK->ilength, buf);
    return;
};
static void rawread_64bit_psfchara(struct psf_bin_work *psf_b_WK, void *buf){
    psf_b_WK->lchar_out = rawread_64bit(psf_b_WK->iflag_keep,
                                        psf_b_WK->ilength, buf);
    return;
};


static struct psf_bin_work * open_read_psf_bin_file(const char *file_name){
    struct psf_bin_work *psf_b_WK = init_psf_bin_work();
    
    int itmp = 0;
    open_rd_rawfile(file_name, &psf_b_WK->ierr);
    psf_b_WK->ilength = sizeof(int);
    psf_b_WK->lchar_out = rawread_32bit(psf_b_WK->iflag_keep,
                                        psf_b_WK->ilength, &itmp);
    if(itmp != psf_b_WK->i_UNIX){psf_b_WK->iflag_swap = 1;};
    
    return psf_b_WK;
};
static void close_read_psf_bin_file(struct psf_bin_work *psf_b_WK){
    close_rawfile();
    dealloc_psf_bin_work(psf_b_WK);
    return;
};

static void read_alloc_psf_node_bin(struct psf_data *psf_b, struct psf_bin_work *psf_b_WK){
    int i, j;
    
    long *n_inter =  (long *) calloc(psf_b_WK->nprocs,sizeof(long));
    psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
    rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
    rawread_64bit_psf(psf_b_WK, n_inter);
    /*
     printf("n_inter ");
     for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", n_inter[i]);};
     printf("\n");
     */
    
    psf_b->nnod_viz = 0;
    for(i=0;i<psf_b_WK->nprocs;i++){psf_b->nnod_viz = psf_b->nnod_viz + n_inter[i];};
    free(n_inter);
    
    alloc_viz_node_s(psf_b);
    double *xx = (double *) calloc(psf_b->nnod_viz,sizeof(double));
    
    for(j=0;j<3;j++){
        psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
        rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
        psf_b_WK->ilength = psf_b->nnod_viz * sizeof(double);
        rawread_64bit_psf(psf_b_WK, xx);
        for(i=0;i<psf_b->nnod_viz;i++){
            psf_b->xyzw_viz[i*IFOUR + j] = xx[i];
        };
    };
    free(xx);
    for(i=0;i<psf_b->nnod_viz;i++){psf_b->inod_viz[i] = i+1;};
    return;
};

static int read_alloc_psf_ele_bin(struct psf_data *psf_b, struct psf_bin_work *psf_b_WK){
	int i, j;
	long eletype;
	int iflag_datatype = IFLAG_SURFACES;
	
	psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &psf_b->nnod_4_ele_viz);
    rawread_64bit_psf(psf_b_WK, &eletype);
    /*    printf("eletype %d \n", eletype); */
	
	if(psf_b->nnod_4_ele_viz == 2){iflag_datatype = IFLAG_LINES;};
	
    long *nele = (long *) calloc(psf_b_WK->nprocs,sizeof(long));
    psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
    rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
    rawread_64bit_psf(psf_b_WK, nele);
    
    psf_b->nele_viz = 0;
    for(i=0;i<psf_b_WK->nprocs;i++){psf_b->nele_viz = psf_b->nele_viz + nele[i];};
    /*
     printf("nele ");
     for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", nele[i]);};
     printf("\n");
     */
    free(nele);
    
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
    return iflag_datatype;
};

static void read_alloc_psf_data_bin(struct psf_data *psf_b, struct psf_bin_work *psf_b_WK){
    int i, j;
    long *n_inter2 =  (long *) calloc(psf_b_WK->nprocs,sizeof(long));
    psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
    rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
    rawread_64bit_psf(psf_b_WK, n_inter2);
    
    long nnod_tmp = 0;
    for(i=0;i<psf_b_WK->nprocs;i++){nnod_tmp = nnod_tmp + n_inter2[i];};
    if(psf_b->nnod_viz != nnod_tmp){
        printf("Number of node is wrong!\n");
        printf("n_inter2 ");
        for(i=0;i<psf_b_WK->nprocs;i++){printf("%ld ", n_inter2[i]);};
        printf("\n");
    };
    
    psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &psf_b->nfield);
    
    alloc_psf_field_name_c(psf_b);
    
    psf_b_WK->ilength = psf_b->nfield*sizeof(long);
    rawread_64bit_psf(psf_b_WK, psf_b->ncomp);
    for(i=0;i<psf_b->nfield;i++){
        psf_b_WK->ilength = (KCHARA_C-1)*sizeof(char);
        rawread_64bit_psfchara(psf_b_WK, psf_b->data_name[i]);
        psf_b->data_name[i] = trim(psf_b->data_name[i]);
        psf_b->id_coord[i] = set_field_coordinate_flag(psf_b->data_name[i]);
    }
    psf_b->istack_comp[0] = 0;
    for(i=0;i<psf_b->nfield;i++){
        psf_b->istack_comp[i+1] = psf_b->istack_comp[i] + psf_b->ncomp[i];
    };
    psf_b->ncomptot = psf_b->istack_comp[psf_b->nfield];
    
    alloc_psf_field_data_c(psf_b);
    alloc_psf_data_s(psf_b);
    double *d_nod = (double *) calloc(psf_b->nnod_viz,sizeof(double));
    
    for(j=0;j<psf_b->ncomptot;j++){
        psf_b_WK->ilength = psf_b_WK->nprocs * sizeof(long);
        rawread_64bit_psf(psf_b_WK, psf_b_WK->itmp_mp);
        psf_b_WK->ilength = psf_b->nnod_viz * sizeof(double);
        rawread_64bit_psf(psf_b_WK, &d_nod[0]);
        for(i=0;i<psf_b->nnod_viz;i++){
            psf_b->d_nod[i*psf_b->ncomptot + j] = d_nod[i];
        };
    };
    return;
};


int read_alloc_psf_mesh_bin(const char *bin_name, struct psf_data *psf_b){
    int iflag_datatype;
    struct psf_bin_work *psf_b_WK = open_read_psf_bin_file(bin_name);
    psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &psf_b_WK->nprocs);
    psf_b_WK->itmp_mp = (long *) calloc(psf_b_WK->nprocs,sizeof(long));
    
    read_alloc_psf_node_bin(psf_b, psf_b_WK);
    iflag_datatype = read_alloc_psf_ele_bin(psf_b, psf_b_WK);
    close_read_psf_bin_file(psf_b_WK);
    return iflag_datatype;
};

int read_alloc_psf_bin(const char *bin_name, double *time, struct psf_data *psf_b){
	long time_step;
	double dt;
    struct psf_bin_work *psf_b_WK = open_read_psf_bin_file(bin_name);
    
    psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &psf_b_WK->nprocs);
	
    rawread_64bit_psf(psf_b_WK, &time_step);
    rawread_64bit_psf(psf_b_WK, time);
    rawread_64bit_psf(psf_b_WK, &dt);
	
	psf_b_WK->itmp_mp = (long *) calloc(psf_b_WK->nprocs,sizeof(long));
	
    read_alloc_psf_data_bin(psf_b, psf_b_WK);
    close_read_psf_bin_file(psf_b_WK);
    return 0;
};

int read_alloc_iso_bin(const char *bin_name, double *time, struct psf_data *psf_b){
    long nprocs2;
	long time_step;
	double dt;
    int iflag_datatype;
    struct psf_bin_work *psf_b_WK = open_read_psf_bin_file(bin_name);
    psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &psf_b_WK->nprocs);
    psf_b_WK->itmp_mp = (long *) calloc(psf_b_WK->nprocs,sizeof(long));
    
    read_alloc_psf_node_bin(psf_b, psf_b_WK);
    iflag_datatype = read_alloc_psf_ele_bin(psf_b, psf_b_WK);
    
    psf_b_WK->ilength = sizeof(long);
    rawread_64bit_psf(psf_b_WK, &nprocs2);
    if(psf_b_WK->nprocs != nprocs2){
        printf("Number of processes is wrong!\n");
    };
    
    rawread_64bit_psf(psf_b_WK, &time_step);
    rawread_64bit_psf(psf_b_WK, time);
    rawread_64bit_psf(psf_b_WK, &dt);
	
    read_alloc_psf_data_bin(psf_b, psf_b_WK);
    close_read_psf_bin_file(psf_b_WK);
    return iflag_datatype;
};


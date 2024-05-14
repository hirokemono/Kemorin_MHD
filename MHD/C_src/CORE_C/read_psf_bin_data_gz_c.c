/*
//  read_psf_bin_data_gz_c.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2020/06/12.
*/

#include "read_psf_bin_data_gz_c.h"

static void gzread_64bit_psf(void *FP_gzip, struct psf_bin_work *psf_z_WK,
							 char *textbuf){
    psf_z_WK->ierr = gzread_64bit_c(FP_gzip, psf_z_WK->iflag_swap,
                                    (int) psf_z_WK->ilength, textbuf);
    return;
};

static void gzread_64bit_psfchara(void *FP_gzip, struct psf_bin_work *psf_z_WK,
								  char *textbuf){
    psf_z_WK->ierr = gzread_64bit_c(FP_gzip, psf_z_WK->iflag_keep,
                                    (int) psf_z_WK->ilength, textbuf);
    return;
};


static void * open_read_psf_bin_gz_file(const char *gzip_name, struct psf_bin_work *psf_z_WK){
    void *FP_gzip = open_rd_gzfile_c(gzip_name);
    int itmp_gz = 0;
    psf_z_WK->ilength = sizeof(int);
    psf_z_WK->ierr = gzread_32bit_c(FP_gzip, psf_z_WK->iflag_keep,
                                    (int) psf_z_WK->ilength, (char *) &itmp_gz);
    if(itmp_gz != psf_z_WK->i_UNIX){psf_z_WK->iflag_swap = 1;};
    
    return FP_gzip;
};
static void close_read_psf_bin_gz_file(void *FP_gzip, struct psf_bin_work *psf_z_WK){
    close_gzfile_c(FP_gzip);
	dealloc_psf_bin_work(psf_z_WK);
/*	free(FP_gzip);*/
    return;
};

static void read_alloc_psf_node_bin_gz(void *FP_gzip, struct psf_data *psf_z,
									   struct psf_bin_work *psf_z_WK){
    int i, j;
    
    long *n_inter_gz =  (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) n_inter_gz);
    
    psf_z->nnod_viz = 0;
    for(i=0;i<psf_z_WK->nprocs;i++){psf_z->nnod_viz = psf_z->nnod_viz + n_inter_gz[i];};
    /*
     printf("n_inter_gz ");
     for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", n_inter_gz[i]);};
     printf("\n");
     */
    free(n_inter_gz);
    
    alloc_viz_node_s(psf_z);
    double *xx_gz = (double *) calloc(psf_z->nnod_viz,sizeof(double));
    
    for(j=0;j<3;j++){
        psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) xx_gz);
        for(i=0;i<psf_z->nnod_viz;i++){
            psf_z->xyzw_viz[i*IFOUR + j] = xx_gz[i];
        };
    };
    free(xx_gz);
    for(i=0;i<psf_z->nnod_viz;i++){psf_z->inod_viz[i] = i+1;};
    return;
};

static int read_alloc_psf_ele_bin_gz(void *FP_gzip, struct psf_data *psf_z,
									 struct psf_bin_work *psf_z_WK){
    int i, j;
    long eletype_gz;
	int iflag_datatype = IFLAG_SURFACES;
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) &psf_z->nnod_4_ele_viz);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) &eletype_gz);
    /*    printf("eletype_gz %d \n", eletype_gz); */
	
	if(psf_z->nnod_4_ele_viz == 2){iflag_datatype = IFLAG_LINES;};
    
    long *nele_gz = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) nele_gz);
    
    psf_z->nele_viz = 0;
    for(i=0;i<psf_z_WK->nprocs;i++){psf_z->nele_viz = psf_z->nele_viz + nele_gz[i];};
    /*
     printf("nele_gz ");
     for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", nele_gz[i]);};
     printf("\n");
     */
    free(nele_gz);
    
    alloc_viz_ele_s(psf_z);
    long *ie_gz = (long *) calloc(psf_z->nele_viz,sizeof(long));
    
    for(j=0;j<psf_z->nnod_4_ele_viz;j++){
        psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nele_viz*sizeof(long);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) ie_gz);
        for(i=0;i<psf_z->nele_viz;i++){
            psf_z->ie_viz[i][j] = ie_gz[i];
        };
    };
    free(ie_gz);
    return iflag_datatype;
};

static void read_alloc_psf_data_bin_gz(void *FP_gzip, struct psf_data *psf_z,
									   struct psf_bin_work *psf_z_WK){
    int i, j;
    long *n_inter2_gz =  (long *) calloc(psf_z_WK->nprocs,sizeof(long));
    
    psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) n_inter2_gz);
    
    long nnod_tmp_gz = 0;
    for(i=0;i<psf_z_WK->nprocs;i++){nnod_tmp_gz = nnod_tmp_gz + n_inter2_gz[i];};
    if(psf_z->nnod_viz != nnod_tmp_gz){
        printf("Number of node is wrong!\n");
        printf("n_inter2_gz ");
        for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", n_inter2_gz[i]);};
        printf("\n");
    };
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) &psf_z->nfield);
    
    alloc_psf_field_name_c(psf_z);
    
    psf_z_WK->ilength = psf_z->nfield*sizeof(long);
    gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z->ncomp);
    for(i=0;i<psf_z->nfield;i++){
        psf_z_WK->ilength = (KCHARA_C-1)*sizeof(char);
        gzread_64bit_psfchara(FP_gzip, psf_z_WK, (char *) psf_z->data_name[i]);
        psf_z->data_name[i] = trim(psf_z->data_name[i]);
        psf_z->id_coord[i] = set_field_coordinate_flag(psf_z->data_name[i]);
    }
    
    psf_z->istack_comp[0] = 0;
    for(i=0;i<psf_z->nfield;i++){
        psf_z->istack_comp[i+1] = psf_z->istack_comp[i] + psf_z->ncomp[i];
    };
    psf_z->ncomptot = psf_z->istack_comp[psf_z->nfield];
    
    alloc_psf_field_data_c(psf_z);
    alloc_psf_data_s(psf_z);
    double *d_nod_gz = (double *) calloc(psf_z->nnod_viz,sizeof(double));
    
    for(j=0;j<psf_z->ncomptot;j++){
        psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
        gzread_64bit_psf(FP_gzip, psf_z_WK, (char *) d_nod_gz);
        for(i=0;i<psf_z->nnod_viz;i++){
            psf_z->d_nod[i*psf_z->ncomptot + j] = d_nod_gz[i];
        };
    };
    free(d_nod_gz);
    return;
};


int read_alloc_psf_mesh_bin_gz(const char *gzip_name, struct psf_data *psf_z){
	int iflag_datatype;
    struct psf_bin_work *psf_z_WK = init_psf_bin_work();
    void *FP_gzip1 = open_read_psf_bin_gz_file(gzip_name, psf_z_WK);
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &psf_z_WK->nprocs);
    psf_z_WK->itmp_mp = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    
    read_alloc_psf_node_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    iflag_datatype = read_alloc_psf_ele_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(FP_gzip1, psf_z_WK);
    return iflag_datatype;
};

int read_alloc_psf_bin_gz(const char *gzip_name, double *time, struct psf_data *psf_z){
	long time_step;
	double dt;
    struct psf_bin_work *psf_z_WK = init_psf_bin_work();
    void *FP_gzip1 = open_read_psf_bin_gz_file(gzip_name, psf_z_WK);
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &psf_z_WK->nprocs);
	
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &time_step);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) time);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &dt);
	
	
	psf_z_WK->itmp_mp = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
	
    read_alloc_psf_data_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(FP_gzip1, psf_z_WK);
    return 0;
};

int read_alloc_iso_bin_gz(const char *gzip_name, double *time, struct psf_data *psf_z){
	long time_step;
	double dt;
	int iflag_datatype;
    struct psf_bin_work *psf_z_WK = init_psf_bin_work();
    void * FP_gzip1 = open_read_psf_bin_gz_file(gzip_name, psf_z_WK);
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &psf_z_WK->nprocs);
    psf_z_WK->itmp_mp = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    
    read_alloc_psf_node_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    iflag_datatype = read_alloc_psf_ele_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    
    long nprocs2_gz = 0;
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &nprocs2_gz);
    if(psf_z_WK->nprocs != nprocs2_gz){
        printf("Number of processes is wrong!\n");
    };
	
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &time_step);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) time);
    gzread_64bit_psf(FP_gzip1, psf_z_WK, (char *) &dt);
    
    read_alloc_psf_data_bin_gz(FP_gzip1, psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(FP_gzip1, psf_z_WK);
    return iflag_datatype;
};


/*
//  read_psf_bin_data_gz_c.c
//  Kemorin_MHD_Cocoa
//
//  Created by Hiroaki Matsui on 2020/06/12.
*/

#include "read_psf_bin_data_gz_c.h"

void gzread_64bit_psf(struct psf_bin_work *psf_z_WK, char *textbuf){
    gzread_64bit_f(&psf_z_WK->iflag_swap, &psf_z_WK->ilength, 
                   textbuf, &psf_z_WK->ierr);
    return;
};

void gzread_64bit_psfchara(struct psf_bin_work *psf_z_WK, char *textbuf){
    gzread_64bit_f(&psf_z_WK->iflag_keep, &psf_z_WK->ilength, 
                   textbuf, &psf_z_WK->ierr);
    return;
};


struct psf_bin_work * open_read_psf_bin_gz_file(const char *gzip_name){
    struct psf_bin_work *psf_z_WK = init_psf_bin_work();
    int itmp_gz = 0;
    open_rd_gzfile(gzip_name);
    psf_z_WK->ilength = sizeof(int);
    gzread_32bit_f(&psf_z_WK->iflag_keep, &psf_z_WK->ilength, 
                   (char *) &itmp_gz, &psf_z_WK->ierr);
    if(itmp_gz != psf_z_WK->i_UNIX){psf_z_WK->iflag_swap = 1;};
    
    return psf_z_WK;
};
void close_read_psf_bin_gz_file(struct psf_bin_work *psf_z_WK){
    close_gzfile();
    dealloc_psf_bin_work(psf_z_WK);
    return;
};

void read_alloc_psf_node_bin_gz(struct psf_data *psf_z, struct psf_bin_work *psf_z_WK){
    int i, j;
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) &psf_z_WK->nprocs);
    /*
     printf("psf_z_WK->nprocs %d \n", psf_z_WK->nprocs);
     */
    psf_z_WK->itmp_mp = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    
    long *n_inter_gz =  (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(psf_z_WK, (char *) n_inter_gz);
    
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
        gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
        gzread_64bit_psf(psf_z_WK, (char *) xx_gz);
        for(i=0;i<psf_z->nnod_viz;i++){
            psf_z->xx_viz[i][j] = xx_gz[i];
        };
    };
    free(xx_gz);
    for(i=0;i<psf_z->nnod_viz;i++){psf_z->inod_viz[i] = i+1;};
    return;
};

void read_alloc_psf_ele_bin_gz(struct psf_data *psf_z, struct psf_bin_work *psf_z_WK){
    int i, j;
    long eletype_gz;
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) &psf_z->nnod_4_ele_viz);
    gzread_64bit_psf(psf_z_WK, (char *) &eletype_gz);
    /*    printf("eletype_gz %d \n", eletype_gz); */
    
    long *nele_gz = (long *)calloc(psf_z_WK->nprocs,sizeof(long));
    psf_z_WK->ilength = psf_z_WK->nprocs * sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(psf_z_WK, (char *) nele_gz);
    
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
        gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nele_viz*sizeof(long);
        gzread_64bit_psf(psf_z_WK, (char *) ie_gz);
        for(i=0;i<psf_z->nele_viz;i++){
            psf_z->ie_viz[i][j] = ie_gz[i];
        };
    };
    free(ie_gz);
    return;
};

void read_alloc_psf_data_bin_gz(struct psf_data *psf_z, struct psf_bin_work *psf_z_WK){
    int i, j;
    
    long nprocs2_gz = 0;
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) &nprocs2_gz);
    if(psf_z_WK->nprocs != nprocs2_gz){
        printf("Number of processes is wrong!\n");
    };
    
    long *n_inter2_gz =  (long *) calloc(psf_z_WK->nprocs,sizeof(long));
    
    psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
    gzread_64bit_psf(psf_z_WK, (char *) n_inter2_gz);
    
    long nnod_tmp_gz = 0;
    for(i=0;i<psf_z_WK->nprocs;i++){nnod_tmp_gz = nnod_tmp_gz + n_inter2_gz[i];};
    if(psf_z->nnod_viz != nnod_tmp_gz){
        printf("Number of node is wrong!\n");
        printf("n_inter2_gz ");
        for(i=0;i<psf_z_WK->nprocs;i++){printf("%ld ", n_inter2_gz[i]);};
        printf("\n");
    };
    
    psf_z_WK->ilength = sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) &psf_z->nfield);
    
    alloc_psf_field_name_c(psf_z);
    
    psf_z_WK->ilength = psf_z->nfield*sizeof(long);
    gzread_64bit_psf(psf_z_WK, (char *) psf_z->ncomp);
    for(i=0;i<psf_z->nfield;i++){
        psf_z_WK->ilength = (KCHARA_C-1)*sizeof(char);
        gzread_64bit_psfchara(psf_z_WK, (char *) psf_z->data_name[i]);
        psf_z->data_name[i] = trim(psf_z->data_name[i]);
    }
    
    psf_z->istack_comp[0] = 0;
    for(i=0;i<psf_z->nfield;i++){
        psf_z->istack_comp[i+1] = psf_z->istack_comp[i] + psf_z->ncomp[i];
    };
    psf_z->ncomptot = psf_z->istack_comp[psf_z->nfield];
    
    alloc_psf_field_data_c(psf_z);
    double *d_nod_gz = (double *) calloc(psf_z->nnod_viz,sizeof(double));
    
    for(j=0;j<psf_z->ncomptot;j++){
        psf_z_WK->ilength = psf_z_WK->nprocs*sizeof(long);
        gzread_64bit_psf(psf_z_WK, (char *) psf_z_WK->itmp_mp);
        psf_z_WK->ilength = psf_z->nnod_viz * sizeof(double);
        gzread_64bit_psf(psf_z_WK, (char *) d_nod_gz);
        for(i=0;i<psf_z->nnod_viz;i++){
            psf_z->d_nod[i][j] = d_nod_gz[i];
        };
    };
    free(d_nod_gz);
    return;
};


void read_alloc_psf_mesh_bin_gz(const char *gzip_name, struct psf_data *psf_z){
    struct psf_bin_work *psf_z_WK = open_read_psf_bin_gz_file(gzip_name);
    read_alloc_psf_node_bin_gz(psf_z, psf_z_WK);
    read_alloc_psf_ele_bin_gz(psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(psf_z_WK);
    return;
};

void read_alloc_psf_bin_gz(const char *gzip_name, struct psf_data *psf_z){
    struct psf_bin_work *psf_z_WK = open_read_psf_bin_gz_file(gzip_name);
    read_alloc_psf_data_bin_gz(psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(psf_z_WK);
    return;
};

void read_alloc_iso_bin_gz(const char *gzip_name, struct psf_data *psf_z){
    struct psf_bin_work *psf_z_WK = open_read_psf_bin_gz_file(gzip_name);
    read_alloc_psf_node_bin_gz(psf_z, psf_z_WK);
    read_alloc_psf_ele_bin_gz(psf_z, psf_z_WK);
    read_alloc_psf_data_bin_gz(psf_z, psf_z_WK);
    close_read_psf_bin_gz_file(psf_z_WK);
    return;
};


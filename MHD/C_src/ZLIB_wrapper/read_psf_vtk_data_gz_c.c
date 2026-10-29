/*
//
//  read_psf_vtk_data_gz_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2013/09/22.
//
*/

#include "read_psf_vtk_data_gz_c.h"


static void read_psf_vtk_num_node_gz(void *FP_gzip, int lbuf, char *buf,
                                      struct psf_data *viz_s){
    char tmpchara[8];
    int num_word, nchara;
    
    num_word = skip_comment_gz_c(FP_gzip, lbuf, buf);
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* ASCII */
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* DATASET UNSTRUCTURED_GRID */
    
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* POINTS    nnod_viz  float */
    sscanf(buf, "%6s %ld %5s", tmpchara, &viz_s->nnod_viz, tmpchara);
    return;
};

static void read_psf_vtk_node_data_gz(void *FP_gzip, int lbuf, char *buf,
                                      struct psf_data *viz_s){
    int num_word, nchara;
    
    alloc_viz_node_s(viz_s);
    
    int i;
    for(i = 0; i < viz_s->nnod_viz; i++) {
        viz_s->inod_viz[i] = i + 1;
        get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
        sscanf(buf, "%lf %lf %lf",
               &viz_s->xyzw_viz[i*IFOUR + 0],
               &viz_s->xyzw_viz[i*IFOUR + 1],
               &viz_s->xyzw_viz[i*IFOUR + 2]);
    };
    return;
};

static int read_psf_vtk_connect_data_gz(void *FP_gzip, int lbuf, char *buf,
                                        struct psf_data *viz_s){
	int num_word, nchara;
	char tmpchara[11];
    int i, j, num_index, itmp;
	int iflag_datatype = IFLAG_SURFACES;
    
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* CELLS    nele_viz  nele*nnod_4_ele */
	sscanf(buf, "%5s %ld %d", tmpchara, &viz_s->nele_viz, &num_index);
    
    viz_s->nnod_4_ele_viz = (num_index / viz_s->nele_viz) - 1;
	alloc_viz_ele_s(viz_s);
    
    if(viz_s->nnod_4_ele_viz == 4){
		printf("Quad patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%d %ld %ld %ld %ld", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
        };
    }else if(viz_s->nnod_4_ele_viz == 3){
		printf("Triangle patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%d %ld %ld %ld", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2]);
        };
    }else if(viz_s->nnod_4_ele_viz == 2){
		printf("Line data \n");
		iflag_datatype = IFLAG_LINES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%d %ld %ld", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1]);
        };
    }else if(viz_s->nnod_4_ele_viz == 1){
		printf("Points data \n");
		iflag_datatype = IFLAG_POINTS;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%d %ld", &itmp, &viz_s->ie_viz[i][0]);
        };
    };
    
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* CELL_TYPES    nele_viz */
	sscanf(buf, "%10s %d", tmpchara, &num_index);
    for (i = 0; i < viz_s->nele_viz; i++) {
        get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
        sscanf(buf, "%d", &itmp);
    };
    for (i = 0; i < viz_s->nele_viz; i++) {
        for (j=0; j<viz_s->nnod_4_ele_viz; j++) {
            viz_s->ie_viz[i][j] = viz_s->ie_viz[i][j]+1;
        }
    };
    return iflag_datatype;
}

static int read_psf_vtk_field_list_gz(void *FP_gzip, int lbuf, char *buf,
                                      vtk_fields_t *fld_list){
	int i;
    double rtmp;
	char fieldtype[8];
	int num_word, nchara;
	
    get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
    if (check_gzfile_eof_c(FP_gzip)) {
        free(fld_list);
        return 1;
    };
    
    sscanf(buf, "%7s %s", fieldtype, fld_list->field_name);
    
    if(   fieldtype[0] == 'T'
       && fieldtype[1] == 'E'
       && fieldtype[2] == 'N'
       && fieldtype[3] == 'S'
       && fieldtype[4] == 'O'
       && fieldtype[5] == 'R'
       && fieldtype[6] == 'S'){
        fld_list->ncomp_vtk = 6;
    } else if(   fieldtype[0] == 'V'
              && fieldtype[1] == 'E'
              && fieldtype[2] == 'C'
              && fieldtype[3] == 'T'
              && fieldtype[4] == 'O'
              && fieldtype[5] == 'R'
              && fieldtype[6] == 'S'){
        fld_list->ncomp_vtk = 3;
    } else if(   fieldtype[0] == 'S'
              && fieldtype[1] == 'C'
              && fieldtype[2] == 'A'
              && fieldtype[3] == 'L'
              && fieldtype[4] == 'A'
              && fieldtype[5] == 'R'
              && fieldtype[6] == 'S'){
        fld_list->ncomp_vtk = 1;
        get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* LOOKUP_TABLE  default */
    } else {
        fld_list->ncomp_vtk = 1;
        get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);     /* LOOKUP_TABLE  default */
    };
    
    alloc_vtk_field_data_c(fld_list);
    
    if(fld_list->ncomp_vtk == 6){
        for (i = 0; i < fld_list->nnod_fld; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%lf %lf %lf",
                   &fld_list->d_vtk[i][0],
                   &fld_list->d_vtk[i][1],
                   &fld_list->d_vtk[i][2]);
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%lf %lf %lf",
                   &rtmp,
                   &fld_list->d_vtk[i][3],
                   &fld_list->d_vtk[i][4]);
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%lf %lf %lf",
                   &rtmp, &rtmp,
                   &fld_list->d_vtk[i][5]);
        };
    } else if(fld_list->ncomp_vtk == 3){
        for (i = 0; i < fld_list->nnod_fld; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%lf %lf %lf",
                   &fld_list->d_vtk[i][0],
                   &fld_list->d_vtk[i][1],
                   &fld_list->d_vtk[i][2]);
        };
    } else {
        for (i = 0; i < fld_list->nnod_fld; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, &num_word, &nchara, buf);
            sscanf(buf, "%lf", &fld_list->d_vtk[i][0]);
        };
    };
    
    fld_list->next_fld = (vtk_fields_t *) malloc(sizeof(vtk_fields_t));
    return 0;
}


static void read_psf_vtk_field_data_gz(void *FP_gzip, int lbuf, char *buf,
                                       struct psf_data *viz_s){
    struct vtk_field vtk_tmp;
    vtk_fields_t *last_fld;
    int iflag_end;
	char tmpchara[200];

    skip_comment_gz_c(FP_gzip, lbuf, buf);  /* POINT_DATA  nnod_viz */
	sscanf(buf, "%10s %d", tmpchara, &vtk_tmp.nnod_vtk);

    vtk_tmp.vtk_fields = (vtk_fields_t *) malloc(sizeof(vtk_fields_t));
    last_fld = vtk_tmp.vtk_fields;
    vtk_tmp.nfld_vtk = -1;
    iflag_end = 0;

    while (iflag_end == 0) {
        last_fld->nnod_fld = vtk_tmp.nnod_vtk;
        iflag_end = read_psf_vtk_field_list_gz(FP_gzip, lbuf, buf, last_fld);
        vtk_tmp.nfld_vtk = vtk_tmp.nfld_vtk + 1;
        last_fld = last_fld->next_fld;
    }

    viz_s->nfield = vtk_tmp.nfld_vtk;
    alloc_psf_field_name_c(viz_s);
    copy_vtk_list_2_udt_name(viz_s, &vtk_tmp);
    
    alloc_psf_field_data_c(viz_s);
    copy_vtk_list_2_udt_data(viz_s, &vtk_tmp);
    
    dealloc_vtk_fields_list_c(&vtk_tmp);
    
	return;
};


void read_num_node_vtg_gz(const char *file_name, struct psf_data *viz_s){
    int lbuf = LENGTHBUF;
    char buf[lbuf];    /* array for reading line */
    
    printf("gzipped grid file name: %s \n",file_name);
    
    /* Error for failed file*/
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
    if(FP_gzip1 == NULL){return;};
    
    read_psf_vtk_num_node_gz(FP_gzip1, lbuf, buf, viz_s);
    
    close_gzfile_c(FP_gzip1);
    return;
}

int read_psf_vtg_gz(const char *file_name, struct psf_data *viz_s){
    int lbuf = LENGTHBUF;
    char buf[lbuf];    /* array for reading line */
    
	printf("gzipped grid file name: %s \n",file_name);
	
	/* Error for failed file*/
	void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if(FP_gzip1 == NULL){return 1;};
	
    read_psf_vtk_num_node_gz(FP_gzip1, lbuf, buf, viz_s);
	read_psf_vtk_node_data_gz(FP_gzip1, lbuf, buf, viz_s);
	int iflag_datatype = read_psf_vtk_connect_data_gz(FP_gzip1, lbuf, buf, viz_s);
	if(iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
	}
    
    close_gzfile_c(FP_gzip1);
	return iflag_datatype;
}

int read_psf_vtd_gz(const char *file_name, struct psf_data *viz_s){
    int lbuf = LENGTHBUF;
    char buf[lbuf];    /* array for reading line */

    printf("gzipped VTD file name: %s \n",file_name);
	
	/* Error for failed file*/
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if (FP_gzip1 == NULL){return 1;};     /* terminate with error message */
	
    read_psf_vtk_field_data_gz(FP_gzip1, lbuf, buf, viz_s);
    close_gzfile_c(FP_gzip1);
    return 0;
}

int read_kemoview_vtk_gz(const char *file_name, struct psf_data *viz_s){
    int lbuf = LENGTHBUF;
    char buf[lbuf];    /* array for reading line */

    printf("gzipped VTK file name: %s \n",file_name);
	
	/* Error for failed file*/
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if (FP_gzip1 == NULL){return 1;};     /* terminate with error message */
	
    read_psf_vtk_num_node_gz(FP_gzip1, lbuf, buf, viz_s);
	read_psf_vtk_node_data_gz(FP_gzip1, lbuf, buf, viz_s);
	int iflag_datatype = read_psf_vtk_connect_data_gz(FP_gzip1, lbuf, buf, viz_s);
    
    read_psf_vtk_field_data_gz(FP_gzip1, lbuf, buf, viz_s);
    
    close_gzfile_c(FP_gzip1);
	return iflag_datatype;
}

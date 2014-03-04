/*
//
//  read_psf_vtk_data_gz_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2013/09/22.
//
*/

#include "read_psf_vtk_data_gz_c.h"

FILE *fp;


static void read_psf_vtk_node_data_gz(struct psf_data *viz_s){
	int i;
	char tmpchara[8];
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
    num_word[0] = skip_comment_gz_c(lbuf, buf);
    get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* ASCII */
    get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* DATASET UNSTRUCTURED_GRID */
    
	get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* POINTS    nnod_viz  float */
	sscanf(buf, "%6s %d %5s", tmpchara, &viz_s->nnod_viz, tmpchara);
	
	alloc_viz_node_s(viz_s);
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
        viz_s->inod_viz[i] = i + 1;
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		sscanf(buf, "%lf %lf %lf",
               &viz_s->xx_viz[i][0],
               &viz_s->xx_viz[i][1],
               &viz_s->xx_viz[i][2]);
	};
	return;
};

static int read_psf_vtk_connect_data_gz(struct psf_data *viz_s){
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	char tmpchara[8];
    int i, j, num_index, iflag_datatype, itmp;
    
	lbuf[0] = LENGTHBUF;
    
	get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* CELLS    nele_viz  nele*nnod_4_ele */
	sscanf(buf, "%5s %d %d", tmpchara, &viz_s->nele_viz, &num_index);
    
    viz_s->nnod_4_ele_viz = (num_index / viz_s->nele_viz) - 1;
	alloc_viz_ele_s(viz_s);
    
    if(viz_s->nnod_4_ele_viz == 4){
		printf("Quad patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz(lbuf, num_word, nchara, buf);
            sscanf(buf, "%d %d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
        };
    }
    else if(viz_s->nnod_4_ele_viz == 3){
		printf("Triangle patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz(lbuf, num_word, nchara, buf);
            sscanf(buf, "%d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2]);
        };
    }
    else if(viz_s->nnod_4_ele_viz == 2){
		printf("Line data \n");
		iflag_datatype = IFLAG_LINES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz(lbuf, num_word, nchara, buf);
            sscanf(buf, "%d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1]);
        };
    };
    
    get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* CELL_TYPES    nele_viz */
	sscanf(buf, "%10s %d", tmpchara, &num_index);
    for (i = 0; i < viz_s->nele_viz; i++) {
        get_one_line_from_gz(lbuf, num_word, nchara, buf);
        sscanf(buf, "%d", &itmp);
    };
    
    for (i = 0; i < viz_s->nele_viz; i++) {
        for (j=0; j<viz_s->nnod_4_ele_viz; j++) {
            viz_s->ie_viz[i][j] = viz_s->ie_viz[i][j]+1;
        }
    };
    return iflag_datatype;
}

static void read_psf_vtk_field_data_gz(struct psf_data *viz_s){
    struct psf_data *vtk_tmp;
    int ifile_end;
	int i, ist;
    double rtmp;
	char tmpchara[8], fieldtype[7];
	char new_field_name[255];
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;

    num_word[0] = skip_comment_gz_c(lbuf, buf);  /* POINT_DATA  nnod_viz */
	sscanf(buf, "%10s %d", tmpchara, &viz_s->nnod_viz);
	
    vtk_tmp = (struct psf_data *) malloc(sizeof(struct psf_data));
    viz_s->nfield = 0;
    viz_s->ncomptot = 0;
    vtk_tmp->nnod_viz = viz_s->nnod_viz;
    vtk_tmp->nfield = 0;
    vtk_tmp->ncomptot = 0;
    ifile_end = 0;
    while (ifile_end < 1) {
        get_one_line_from_gz(lbuf, num_word, nchara, buf);
        if (check_gzfile_eof()) {ifile_end = 1; break;}
        
        
        sscanf(buf, "%7s %s", fieldtype, new_field_name);
        
        if (viz_s->nfield > 0) {
            vtk_tmp->nfield = viz_s->nfield;
            alloc_psf_field_name_c(vtk_tmp);
            copy_viewer_udt_field_name(vtk_tmp, viz_s);
            
            alloc_psf_field_data_c(vtk_tmp);
            copy_viewer_udt_data(vtk_tmp, viz_s);
            dealloc_psf_field_data_c(viz_s);
        }
        
        viz_s->nfield = vtk_tmp->nfield + 1;
        alloc_psf_field_name_c(viz_s);
        if (viz_s->nfield > 1) copy_viewer_udt_field_name(viz_s, vtk_tmp);
        strngcopy(viz_s->data_name[viz_s->nfield-1], new_field_name);
        if(       fieldtype[0] == 'T'
           && fieldtype[1] == 'E'
           && fieldtype[2] == 'N'
           && fieldtype[3] == 'S'
           && fieldtype[4] == 'O'
           && fieldtype[5] == 'R'
           && fieldtype[6] == 'S'){
            viz_s->ncomp[viz_s->nfield-1] = 6;
        } else if(fieldtype[0] == 'V'
                  && fieldtype[1] == 'E'
                  && fieldtype[2] == 'C'
                  && fieldtype[3] == 'T'
                  && fieldtype[4] == 'O'
                  && fieldtype[5] == 'R'
                  && fieldtype[6] == 'S'){
            viz_s->ncomp[viz_s->nfield-1] = 3;
        } else if(fieldtype[0] == 'S'
                  && fieldtype[1] == 'C'
                  && fieldtype[2] == 'A'
                  && fieldtype[3] == 'L'
                  && fieldtype[4] == 'A'
                  && fieldtype[5] == 'R'
                  && fieldtype[6] == 'S'){
            viz_s->ncomp[viz_s->nfield-1] = 1;
            get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* LOOKUP_TABLE  default */
        } else {
            viz_s->ncomp[viz_s->nfield-1] = 1;
            get_one_line_from_gz(lbuf, num_word, nchara, buf);     /* LOOKUP_TABLE  default */
        };
/*        printf("ncomp_app %d \n",viz_s->ncomp[viz_s->nfield-1]);*/
        viz_s->istack_comp[viz_s->nfield] = viz_s->istack_comp[viz_s->nfield-1]
        + viz_s->ncomp[viz_s->nfield-1];
        viz_s->ncomptot = viz_s->istack_comp[viz_s->nfield];
        alloc_psf_field_data_c(viz_s);
        ist = vtk_tmp->ncomptot;
        
        if (viz_s->nfield > 1) {
            copy_viewer_udt_data(viz_s, vtk_tmp);
            dealloc_psf_field_data_c(vtk_tmp);
        };
        
        if(viz_s->ncomp[viz_s->nfield-1] == 6){
            for (i = 0; i < viz_s->nnod_viz; i++) {
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
                sscanf(buf, "%lf %lf %lf",
                       &viz_s->d_nod[i][ist  ],
                       &viz_s->d_nod[i][ist+1],
                       &viz_s->d_nod[i][ist+2]);
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
                sscanf(buf, "%lf %lf %lf",
                       &rtmp,
                       &viz_s->d_nod[i][ist+3],
                       &viz_s->d_nod[i][ist+4]);
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
                sscanf(buf, "%lf %lf %lf",
                       &rtmp, &rtmp,
                       &viz_s->d_nod[i][ist+5]);
            };
        } else if(viz_s->ncomp[viz_s->nfield-1] == 3){
            for (i = 0; i < viz_s->nnod_viz; i++) {
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
                sscanf(buf, "%lf %lf %lf",
                       &viz_s->d_nod[i][ist  ],
                       &viz_s->d_nod[i][ist+1],
                       &viz_s->d_nod[i][ist+2]);
            };
        } else {
            for (i = 0; i < viz_s->nnod_viz; i++) {
                get_one_line_from_gz(lbuf, num_word, nchara, buf);
                sscanf(buf, "%lf", &viz_s->d_nod[i][ist]);
            };
        };
        
    }
    alloc_psf_data_s(viz_s);
    free(vtk_tmp);
    
	return;
};


int read_psf_vtg_gz(const char *file_head, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	int ierr, iflag_datatype;
	
	/* printf("file header in: %s \n", file_head); */
	sprintf(file_name, "%s.0.vtg.gz",file_head);
	printf("gzipped grid file name: %s \n",file_name);
	
	/* Error for failed file*/
	ierr = open_rd_gzfile_w_flag(file_name);
	if (ierr == 1){
		return ierr;
	}
	
	read_psf_vtk_node_data_gz(viz_s);
	iflag_datatype = read_psf_vtk_connect_data_gz(viz_s);
	if(iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
	}
    
	close_gzfile();
	return iflag_datatype;
}

int read_psf_vtd_gz(const char *file_head, int istep, struct psf_data *viz_s){
    int ierr;
	char file_name[LENGTHBUF];
	
	sprintf(file_name, "%s.%d.vtd.gz",file_head,istep);
	printf("UDT file name: %s \n",file_name);
	
	/* Error for failed file*/
	ierr = open_rd_gzfile_w_flag(file_name);
	if (ierr == 1){
		return ierr;
	}
	
	read_psf_vtk_field_data_gz(viz_s);
    
	close_gzfile();
	return 0;
}

int read_kemoview_vtk_gz(const char *file_head, struct psf_data *viz_s){
	int iflag_datatype, ierr;
	char file_name[LENGTHBUF];
	
	sprintf(file_name, "%s.vtk.gz",file_head);
	printf("UCD file name: %s \n",file_name);
	
	/* Error for failed file*/
	ierr = open_rd_gzfile_w_flag(file_name);
	if (ierr == 1){
		fprintf(stderr, "Cannot open file!\n");
		return -1;                    /* terminate with error message */
	};
	
	read_psf_vtk_node_data_gz(viz_s);
	iflag_datatype = read_psf_vtk_connect_data_gz(viz_s);
	
	read_psf_vtk_field_data_gz(viz_s);
	close_gzfile();
	return iflag_datatype;
}

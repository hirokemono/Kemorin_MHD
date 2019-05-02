/*
//  read_psf_vtk_data_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 13/01/12.
//
*/

#include "read_psf_vtk_data_c.h"

FILE *fp;


static void read_psf_vtk_node_data(struct psf_data *viz_s){
	int i;
    long offset;
	char tmpchara[8];
	char buf[LENGTHBUF];    /* array for reading line */
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);     /* (blank line) */
    fgets(buf, LENGTHBUF, fp);     /* ASCII */
    fgets(buf, LENGTHBUF, fp);     /* DATASET UNSTRUCTURED_GRID */

	fgets(buf, LENGTHBUF, fp);     /* POINTS    nnod_viz  float */
	sscanf(buf, "%6s %d %5s", tmpchara, &viz_s->nnod_viz, tmpchara);
	
	alloc_viz_node_s(viz_s);
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
        viz_s->inod_viz[i] = i + 1;
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%lf %lf %lf",
               &viz_s->xx_viz[i][0],
               &viz_s->xx_viz[i][1],
               &viz_s->xx_viz[i][2]);
	};
	return;
};

static int read_psf_vtk_connect_data(struct psf_data *viz_s){
	char buf[LENGTHBUF];    /* array for reading line */
	char tmpchara[8];
    int i, j, num_index, iflag_datatype, itmp;


	fgets(buf, LENGTHBUF, fp);     /* CELLS    nele_viz  nele*nnod_4_ele */
	sscanf(buf, "%5s %d %d", tmpchara, &viz_s->nele_viz, &num_index);
    
    viz_s->nnod_4_ele_viz = (num_index / viz_s->nele_viz) - 1;
	alloc_viz_ele_s(viz_s);
    
    if(viz_s->nnod_4_ele_viz == 4){
		printf("Quad patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%d %d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
        };
    }
    else if(viz_s->nnod_4_ele_viz == 3){
		printf("Triangle patch data \n");
		iflag_datatype = IFLAG_SURFACES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2]);
        };
    }
    else if(viz_s->nnod_4_ele_viz == 2){
		printf("Line data \n");
		iflag_datatype = IFLAG_LINES;
        for (i = 0; i < viz_s->nele_viz; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1]);
        };
    };

    fgets(buf, LENGTHBUF, fp);     /* CELL_TYPES    nele_viz */
	sscanf(buf, "%10s %d", tmpchara, &num_index);
    for (i = 0; i < viz_s->nele_viz; i++) {
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%d", &itmp);
    };

    for (i = 0; i < viz_s->nele_viz; i++) {
        for (j=0; j<viz_s->nnod_4_ele_viz; j++) {
            viz_s->ie_viz[i][j] = viz_s->ie_viz[i][j]+1;
        }
    };
    return iflag_datatype;
}


static int read_psf_vtk_field_list(vtk_fields_t *fld_list){
	int i;
    double rtmp;
	char fieldtype[8];
	char buf[LENGTHBUF];    /* array for reading line */
    
    if(fgets(buf, LENGTHBUF, fp) == NULL){
        free(fld_list);
        return 1;
    };

    sscanf(buf, "%7s %s", fieldtype, fld_list->field_name);
    
    if(          fieldtype[0] == 'T'
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
        fgets(buf, LENGTHBUF, fp);     /* LOOKUP_TABLE  default */
    } else {
        fld_list->ncomp_vtk = 1;
        fgets(buf, LENGTHBUF, fp);     /* LOOKUP_TABLE  default */
    };
    
    alloc_vtk_field_data_c(fld_list);
    
    if(fld_list->ncomp_vtk == 6){
        for (i = 0; i < fld_list->nnod_fld; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%lf %lf %lf",
                   &fld_list->d_vtk[i][0],
                   &fld_list->d_vtk[i][1],
                   &fld_list->d_vtk[i][2]);
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%lf %lf %lf",
                   &rtmp,
                   &fld_list->d_vtk[i][3],
                   &fld_list->d_vtk[i][4]);
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%lf %lf %lf",
                   &rtmp, &rtmp,
                   &fld_list->d_vtk[i][5]);
        };
    } else if(fld_list->ncomp_vtk == 3){
        for (i = 0; i < fld_list->nnod_fld; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%lf %lf %lf",
                   &fld_list->d_vtk[i][0],
                   &fld_list->d_vtk[i][1],
                   &fld_list->d_vtk[i][2]);
        };
    } else {
        for (i = 0; i < fld_list->nnod_fld; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%lf", &fld_list->d_vtk[i][0]);
        };
    };
    
    fld_list->next_fld = (vtk_fields_t *) malloc(sizeof(vtk_fields_t));
    return 0;
}

static void read_psf_vtk_field_data(struct psf_data *viz_s){
    struct vtk_field vtk_tmp;
    vtk_fields_t *last_fld;
    int iflag_end;
    long offset;
	char tmpchara[200];
	char buf[LENGTHBUF];    /* array for reading line */
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);     /* POINT_DATA  nnod_viz */
	sscanf(buf, "%10s %d", tmpchara, &vtk_tmp.nnod_vtk);
    vtk_tmp.vtk_fields = (vtk_fields_t *) malloc(sizeof(vtk_fields_t));
    last_fld = vtk_tmp.vtk_fields;
	
    vtk_tmp.nfld_vtk = -1;
    iflag_end = 0;
    while (iflag_end == 0) {
        last_fld->nnod_fld = vtk_tmp.nnod_vtk;
        iflag_end = read_psf_vtk_field_list(last_fld);
        vtk_tmp.nfld_vtk = vtk_tmp.nfld_vtk + 1;
        last_fld = last_fld->next_fld;
    }
    
    viz_s->nfield = vtk_tmp.nfld_vtk;
    alloc_psf_field_name_c(viz_s);
    copy_vtk_list_2_udt_name(viz_s, &vtk_tmp);
    
    alloc_psf_field_data_c(viz_s);
    copy_vtk_list_2_udt_data(viz_s, &vtk_tmp);
    
    dealloc_vtk_fields_list_c(&vtk_tmp);
    alloc_psf_data_s(viz_s);
	return;
};


int read_psf_vtg(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("grid file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return 1;                    /* terminate with error message */
	};
	
	read_psf_vtk_node_data(viz_s);
	iflag_datatype = read_psf_vtk_connect_data(viz_s);
	if(iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
	}
    
	fclose(fp);
	return iflag_datatype;
}

int read_psf_vtd(const char *file_name, struct psf_data *viz_s){
	printf("UDT file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return 1;                    /* terminate with error message */
	};
	
	read_psf_vtk_field_data(viz_s);
    
	fclose(fp);
	return 0;
}

int read_kemoview_vtk(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("VTK file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return -1;                    /* terminate with error message */
	};
	
	read_psf_vtk_node_data(viz_s);
	iflag_datatype = read_psf_vtk_connect_data(viz_s);
	
	read_psf_vtk_field_data(viz_s);
    printf("TAkoTAko");
	fclose(fp);
	return iflag_datatype;
}

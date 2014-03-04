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

static void read_psf_vtk_field_data(struct psf_data *viz_s){
    struct psf_data *vtk_tmp;
    int ifile_end;
	int i, ist;
    double rtmp;
    long offset;
	char tmpchara[8], fieldtype[7];
	char new_field_name[255];
	char buf[LENGTHBUF];    /* array for reading line */
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);     /* POINT_DATA  nnod_viz */
	sscanf(buf, "%10s %d", tmpchara, &viz_s->nnod_viz);
	
    vtk_tmp = (struct psf_data *) malloc(sizeof(struct psf_data));
    viz_s->nfield = 0;
    viz_s->ncomptot = 0;
    vtk_tmp->nnod_viz = viz_s->nnod_viz;
    vtk_tmp->nfield = 0;
    vtk_tmp->ncomptot = 0;
    ifile_end = 0;
    while (ifile_end < 1) {
        if(fgets(buf, LENGTHBUF, fp) == NULL){     /* TENSORS  nnod_viz */
            ifile_end = 1;
            break;
        };
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
            fgets(buf, LENGTHBUF, fp);     /* LOOKUP_TABLE  default */
       } else {
            viz_s->ncomp[viz_s->nfield-1] = 1;
           fgets(buf, LENGTHBUF, fp);     /* LOOKUP_TABLE  default */
        };
        
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
                fgets(buf, LENGTHBUF, fp);
                sscanf(buf, "%lf %lf %lf",
                       &viz_s->d_nod[i][ist  ],
                       &viz_s->d_nod[i][ist+1],
                       &viz_s->d_nod[i][ist+2]);
                fgets(buf, LENGTHBUF, fp);
                sscanf(buf, "%lf %lf %lf",
                       &rtmp,
                       &viz_s->d_nod[i][ist+3],
                       &viz_s->d_nod[i][ist+4]);
                fgets(buf, LENGTHBUF, fp);
                sscanf(buf, "%lf %lf %lf",
                       &rtmp, &rtmp,
                       &viz_s->d_nod[i][ist+5]);
            };
        } else if(viz_s->ncomp[viz_s->nfield-1] == 3){
            for (i = 0; i < viz_s->nnod_viz; i++) {
                fgets(buf, LENGTHBUF, fp);
                sscanf(buf, "%lf %lf %lf",
                       &viz_s->d_nod[i][ist  ],
                       &viz_s->d_nod[i][ist+1],
                       &viz_s->d_nod[i][ist+2]);
            };
        } else {
            for (i = 0; i < viz_s->nnod_viz; i++) {
                fgets(buf, LENGTHBUF, fp);
                sscanf(buf, "%lf", &viz_s->d_nod[i][ist]);
            };
        };
        
    }
    alloc_psf_data_s(viz_s);
    free(vtk_tmp);
    
	return;
};


int read_psf_vtg(const char *file_head, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	int iflag_datatype;
	
	/* printf("file header in: %s \n", file_head); */
	sprintf(file_name, "%s.0.vtg",file_head);
	printf("grid file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
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

int read_psf_vtd(const char *file_head, int istep, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	
	sprintf(file_name, "%s.%d.vtd",file_head,istep);
	printf("UDT file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		return 1;                    /* terminate with error message */
	};
	
	read_psf_vtk_field_data(viz_s);
    
	fclose(fp);
	return 0;
}

int read_kemoview_vtk(const char *file_head, struct psf_data *viz_s){
	int iflag_datatype;
	char file_name[LENGTHBUF];
	
	sprintf(file_name, "%s.vtk",file_head);
	printf("UCD file name: %s \n",file_name);
	
	/* Error for failed file*/
	if ((fp = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		return -1;                    /* terminate with error message */
	};
	
	read_psf_vtk_node_data(viz_s);
	iflag_datatype = read_psf_vtk_connect_data(viz_s);
	
	read_psf_vtk_field_data(viz_s);
	fclose(fp);
	return iflag_datatype;
}

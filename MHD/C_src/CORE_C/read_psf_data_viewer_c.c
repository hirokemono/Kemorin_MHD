
/* read_psf_data_viewer_c.c */

#include "read_psf_data_viewer_c.h"

FILE *fp_psf;


static void read_viz_node_data(struct psf_data *viz_s){
	int i;
	int itmp;
	char buf[LENGTHBUF];    /* array for reading line */
	
	fgets(buf, LENGTHBUF, fp_psf);
	sscanf(buf, "%ld %ld %ld %d %d",
			&viz_s->nnod_viz, &viz_s->nele_viz, 
			&viz_s->ncomptot, &itmp, &itmp);
	
	alloc_viz_node_s(viz_s);
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
		fgets(buf, LENGTHBUF, fp_psf);
		sscanf(buf, "%ld %lf %lf %lf",
			&viz_s->inod_viz[i], &viz_s->xyzw_viz[i*IFOUR + 0],
			&viz_s->xyzw_viz[i*IFOUR + 1], &viz_s->xyzw_viz[i*IFOUR + 2]);
	};
	return;
};

static int read_kemoview_ucd_connect(struct psf_data *viz_s){
	int iflag_datatype;
	int i, itmp;
	char celllabel[5];      /* array for cell label */
	char buf[LENGTHBUF];    /* array for reading line */
	
	iflag_datatype = 0;
	fgets(buf, LENGTHBUF, fp_psf);
	sscanf(buf, "%d %d %4s", &itmp, &itmp, celllabel);
	if(			   celllabel[0] == 't' 
				&& celllabel[1] == 'r'
				&& celllabel[2] == 'i'){
		printf("Triangle patch data \n");
		iflag_datatype = IFLAG_SURFACES;
		viz_s->nnod_4_ele_viz = 3;
	} else if(	   celllabel[0] == 'l' 
				&& celllabel[1] == 'i'
				&& celllabel[2] == 'n'
				&& celllabel[3] == 'e'){
		printf("Line data \n");
		iflag_datatype = IFLAG_LINES;
		viz_s->nnod_4_ele_viz = 2;
	} else if(   celllabel[0] == 'q' 
			  && celllabel[1] == 'u'
			  && celllabel[2] == 'a'
			  && celllabel[3] == 'd'){
		printf("Quad patch data \n");
		iflag_datatype = IFLAG_SURFACES;
		viz_s->nnod_4_ele_viz = 4;
	};
	
	alloc_viz_ele_s(viz_s);
	
	if(viz_s->nnod_4_ele_viz == 3){
		sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp,
				&viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
			fgets(buf, LENGTHBUF, fp_psf);
			sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
		};
	}
	else if(viz_s->nnod_4_ele_viz == 2){
		sscanf(buf, "%d %d line %ld %ld", &itmp, &itmp,
				&viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
			fgets(buf, LENGTHBUF, fp_psf);
			sscanf(buf, "%d %d line %ld %ld", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1]);
		}
	}
	else if(viz_s->nnod_4_ele_viz == 4){
		sscanf(buf, "%d %d line %ld %ld %ld %ld", &itmp, &itmp,
			   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1],
			   &viz_s->ie_viz[0][2], &viz_s->ie_viz[0][3]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
			fgets(buf, LENGTHBUF, fp_psf);
			sscanf(buf, "%d %d line %ld %ld %ld %ld", &itmp, &itmp, 
				   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1],
				   &viz_s->ie_viz[0][2], &viz_s->ie_viz[0][3]);
		}
	};
	
	return iflag_datatype;
};

static int read_psf_connect_data(struct psf_data *viz_s){
	int i;
	int itmp;
	char celllabel[4];    /* array for cell label */
	char buf[LENGTHBUF];    /* array for reading line */
	
	fgets(buf, LENGTHBUF, fp_psf);
	sscanf(buf, "%d %d %3s", &itmp, &itmp, celllabel);
	if(   celllabel[0] != 't' 
	   || celllabel[1] != 'r'
	   || celllabel[2] != 'i') return -1;
	
	viz_s->nnod_4_ele_viz = 3;
	alloc_viz_ele_s(viz_s);
	
	sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp,
		   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);

	for (i = 1; i < viz_s->nele_viz; i++) {
		fgets(buf, LENGTHBUF, fp_psf);
		sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp, 
			&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
	};
	return IFLAG_SURFACES;
};


static void read_viz_phys_data(struct psf_data *viz_s){
	int i, j, itmp;
	char buf[LENGTHBUF];    /* array for reading line */
	
	fscanf(fp_psf, "%ld", &viz_s->nfield);
	
	alloc_psf_field_name_c(viz_s);
	
	for (i = 0; i < viz_s->nfield; i++) {
		fscanf(fp_psf, "%ld", &viz_s->ncomp[i]);
		/*printf("ncomp: %d \n", viz_s->ncomp[i]);*/
	};
	fgets(buf, LENGTHBUF, fp_psf);
	
	viz_s->istack_comp[0] = 0;
	for (i = 0; i < viz_s->nfield; i++) {
		viz_s->istack_comp[i+1]
		= viz_s->istack_comp[i] + viz_s->ncomp[i];
	};
	viz_s->ncomptot = viz_s->istack_comp[viz_s->nfield];
	
    alloc_psf_field_data_c(viz_s);
	alloc_psf_data_s(viz_s);
	
	/* read field name */
	
	read_field_names(fp_psf, viz_s->nfield, viz_s->data_name, viz_s->id_coord);
	
	/*  read field */
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
		fscanf(fp_psf, "%d", &itmp); 
		for (j = 0; j < viz_s->ncomptot; j++){
			fscanf(fp_psf, "%lf", &viz_s->d_nod[i*viz_s->ncomptot + j]);
		};
	};
	return;
};


int read_psf_grd(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("grid file name: %s \n",file_name);
	
	/* Error for failed file*/ 	
	if ((fp_psf = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return 1;                    /* terminate with error message */
	};
	
	read_viz_node_data(viz_s);
	iflag_datatype = read_psf_connect_data(viz_s);
	if(iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
	}

	fclose(fp_psf);
	return iflag_datatype;
}

int read_psf_udt(const char *file_name, struct psf_data *viz_s){
	printf("UDT file name: %s \n",file_name);
	
	/* Error for failed file*/ 	
	if ((fp_psf = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return 1;                    /* terminate with error message */
	};
	
	read_viz_phys_data(viz_s);

	fclose(fp_psf);
	return 0;
}

int read_kemoview_ucd(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("UCD file name: %s \n",file_name);
	
	/* Error for failed file*/ 	
	if ((fp_psf = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!: %s\n", file_name);
		return -1;                    /* terminate with error message */
	};
	
	read_viz_node_data(viz_s);
	iflag_datatype = read_kemoview_ucd_connect(viz_s);
	
	read_viz_phys_data(viz_s);
	fclose(fp_psf);
	return iflag_datatype;
}


/* read_psf_data_gz_c.c */

#include "read_psf_data_gz_c.h"

static void read_gz_viz_node_data(void *FP_gzip, struct psf_data *viz_s){
	int i;
	int itmp;
    int lbuf = LENGTHBUF;
	char buf[lbuf];    /* array for reading line */
	int num_word[1], nchara[1];
	
    get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
	sscanf(buf, "%ld %ld %ld %d %d",
			&viz_s->nnod_viz, &viz_s->nele_viz, 
			&viz_s->ncomptot, &itmp, &itmp);
	
	alloc_viz_node_s(viz_s);
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
        get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
		sscanf(buf, "%ld %lf %lf %lf",
			&viz_s->inod_viz[i], &viz_s->xyzw_viz[i*IFOUR + 0],
			&viz_s->xyzw_viz[i*IFOUR + 1], &viz_s->xyzw_viz[i*IFOUR + 2]);
	};
	return;
};

static int read_gz_kemoview_connect_data(void *FP_gzip, struct psf_data *viz_s){
	int i, iflag_datatype;
	int itmp;
    int lbuf = LENGTHBUF;
	char celllabel[5];    /* array for cell label */
	char buf[lbuf];    /* array for reading line */
	int num_word[1], nchara[1];

    get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
	sscanf(buf, "%d %d %4s", &itmp, &itmp, celllabel);
	iflag_datatype = 0;
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
		printf("Line data \n");
		iflag_datatype = IFLAG_SURFACES;
		viz_s->nnod_4_ele_viz = 4;
	};
	
	alloc_viz_ele_s(viz_s);
	
	if(viz_s->nnod_4_ele_viz == 3){
		sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp,
		   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
	
		for (i = 1; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
		};
	}
	
	else if(viz_s->nnod_4_ele_viz == 2){
		sscanf(buf, "%d %d line %ld %ld", &itmp, &itmp,
				&viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d line %ld %ld", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1]);
		};
	}
	
	else if(viz_s->nnod_4_ele_viz == 4){
		sscanf(buf, "%d %d quad %ld %ld %ld %ld", &itmp, &itmp,
			   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1],
			   &viz_s->ie_viz[0][2], &viz_s->ie_viz[0][3]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
            get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d quad %ld %ld %ld %ld", &itmp, &itmp, 
				   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
				   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
		};
	};
	return iflag_datatype;
};

static int read_gz_psf_connect_data(void *FP_gzip, struct psf_data *viz_s){
	int i;
	int itmp;
    int lbuf = LENGTHBUF;
	char celllabel[4];    /* array for cell label */
	char buf[lbuf];    /* array for reading line */
	int num_word[1], nchara[1];
	
	
    get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
	sscanf(buf, "%d %d %3s", &itmp, &itmp, celllabel);
	if(   celllabel[0] != 't' 
	   || celllabel[1] != 'r'
	   || celllabel[2] != 'i') return -1;
	
	viz_s->nnod_4_ele_viz = 3;
	alloc_viz_ele_s(viz_s);
	
	sscanf(buf, "%d %d %3s %ld %ld %ld", &itmp, &itmp, celllabel,
		   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
	
	for (i = 1; i < viz_s->nele_viz; i++) {
        get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
		sscanf(buf, "%d %d tri %ld %ld %ld", &itmp, &itmp, 
			&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
	};
	return IFLAG_SURFACES;
};


static void read_gz_viz_phys_data(void *FP_gzip, struct psf_data *viz_s){
	int i, j, itmp, iread, nread;
    int lbuf = LENGTHBUF;
	char buf[lbuf];    /* array for reading line */
	int num_word[1], nchara[1];

	iread = 0;
    get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
	sscanf(buf, "%ld%n", &viz_s->nfield, &nread);
	iread = iread + nread;
	
	alloc_psf_field_name_c(viz_s);
	j = 0;
	for (i = 0; i < num_word[0]-1; i++) {
		sscanf(&buf[iread], "%ld%n", &viz_s->ncomp[j], &nread);
		j = j+1;
		iread = iread + nread;
	}

    while (j < viz_s->nfield-1) {
		iread = 0;
        get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
        
		for (i = 0; i < num_word[0]; i++) {
			sscanf(&buf[iread], "%ld%n", &viz_s->ncomp[j], &nread);
			j = j+1;
			iread = iread + nread;
			/*printf("ncomp: %d \n", viz_s->ncomp[i]); */
		};
	};
	
	
	viz_s->istack_comp[0] = 0;
	for (i = 0; i < viz_s->nfield; i++) {
		viz_s->istack_comp[i+1]
		= viz_s->istack_comp[i] + viz_s->ncomp[i];
	};
	viz_s->ncomptot = viz_s->istack_comp[viz_s->nfield];
	
    alloc_psf_field_data_c(viz_s);
	alloc_psf_data_s(viz_s);
	
	/* read field name */
	
	for (i = 0; i < viz_s->nfield; i++) {
        get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
        
        viz_s->id_coord[i] = read_field_name_from_buffer((lbuf+1), buf, viz_s->data_name[i]);
/*		printf("%d, %s coordinate: %d \n", i, viz_s->data_name[i], viz_s->id_coord[i]);*/
	};
	/*  read field */
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
        get_one_line_from_gz_c(FP_gzip, lbuf, num_word, nchara, buf);
        
		iread = 0;
		sscanf(buf, "%d%n", &itmp, &nread); 
		iread = iread + nread;
		for (j = 0; j < viz_s->ncomptot; j++){
			sscanf(&buf[iread], "%lf%n", &viz_s->d_nod[i*viz_s->ncomptot + j], &nread);
			iread = iread + nread;
		};
	};
    
	return;
};


int read_psf_grd_gz(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("gzipped grd file name: %s \n",file_name);
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if (FP_gzip1 == NULL){return 1;};
	
	read_gz_viz_node_data(FP_gzip1, viz_s);
	iflag_datatype = read_gz_psf_connect_data(FP_gzip1, viz_s);
	if (iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
		return iflag_datatype;
	}
	
    close_gzfile_c(FP_gzip1);
	return IFLAG_SURFACES;
}

int read_psf_udt_gz(const char *file_name, struct psf_data *viz_s){
	printf("gzipped udt file name: %s \n",file_name);
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if (FP_gzip1 == NULL){
		return 2;
	}
	
	read_gz_viz_phys_data(FP_gzip1, viz_s);
    close_gzfile_c(FP_gzip1);
	return 0;
}

int read_kemoview_ucd_gz(const char *file_name, struct psf_data *viz_s){
	int iflag_datatype;
	printf("gzipped UCD file name: %s \n",file_name);
    void *FP_gzip1 = open_rd_gzfile_c(file_name);
	if (FP_gzip1 == NULL) return -1;
	
	read_gz_viz_node_data(FP_gzip1, viz_s);
	iflag_datatype = read_gz_kemoview_connect_data(FP_gzip1, viz_s);
	if (iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
		return iflag_datatype;
	}

    read_gz_viz_phys_data(FP_gzip1, viz_s);
    close_gzfile_c(FP_gzip1);
    
	return iflag_datatype;
}

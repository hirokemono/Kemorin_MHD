
/* read_psf_data_gz_c.c */

#include "read_psf_data_gz_c.h"

static void read_gz_viz_node_data(struct psf_data *viz_s){
	int i;
	int itmp;
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	get_one_line_from_gz(lbuf, num_word, nchara, buf);
	sscanf(buf, "%d %d %d %d %d",
			&viz_s->nnod_viz, &viz_s->nele_viz, 
			&viz_s->ncomptot, &itmp, &itmp);
	
	alloc_viz_node_s(viz_s);
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		sscanf(buf, "%d %lf %lf %lf",
			&viz_s->inod_viz[i], &viz_s->xx_viz[i][0], 
			&viz_s->xx_viz[i][1], &viz_s->xx_viz[i][2]);
	};
	return;
};

static int read_gz_kemoview_connect_data(struct psf_data *viz_s){
	int i, iflag_datatype;
	int itmp;
	char celllabel[4];    /* array for cell label */
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	get_one_line_from_gz(lbuf, num_word, nchara, buf);
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
		printf("Line data \n");
		iflag_datatype = IFLAG_SURFACES;
		viz_s->nnod_4_ele_viz = 4;
	};
	
	alloc_viz_ele_s(viz_s);
	
	if(viz_s->nnod_4_ele_viz == 3){
		sscanf(buf, "%d %d tri %d %d %d", &itmp, &itmp,
		   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
	
		for (i = 1; i < viz_s->nele_viz; i++) {
			get_one_line_from_gz(lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d tri %d %d %d", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
		};
	}
	
	else if(viz_s->nnod_4_ele_viz == 2){
		sscanf(buf, "%d %d line %d %d %d", &itmp, &itmp,
				&viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
			get_one_line_from_gz(lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d line %d %d %d", &itmp, &itmp, 
					&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
		};
	}
	
	else if(viz_s->nnod_4_ele_viz == 4){
		sscanf(buf, "%d %d quad %d %d %d %d", &itmp, &itmp,
			   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1],
			   &viz_s->ie_viz[0][2], &viz_s->ie_viz[0][3]);
		
		for (i = 1; i < viz_s->nele_viz; i++) {
			get_one_line_from_gz(lbuf, num_word, nchara, buf);
			sscanf(buf, "%d %d quad %d %d %d %d", &itmp, &itmp, 
				   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
				   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
		};
	};
	return iflag_datatype;
};

static int read_gz_psf_connect_data(struct psf_data *viz_s){
	int i;
	int itmp;
	char celllabel[3];    /* array for cell label */
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	get_one_line_from_gz(lbuf, num_word, nchara, buf);
	sscanf(buf, "%d %d %3s", &itmp, &itmp, celllabel);
	if(   celllabel[0] != 't' 
	   || celllabel[1] != 'r'
	   || celllabel[2] != 'i') return -1;
	
	viz_s->nnod_4_ele_viz = 3;
	alloc_viz_ele_s(viz_s);
	
	sscanf(buf, "%d %d %3s %d %d %d", &itmp, &itmp, celllabel,
		   &viz_s->ie_viz[0][0], &viz_s->ie_viz[0][1], &viz_s->ie_viz[0][2]);
	
	for (i = 1; i < viz_s->nele_viz; i++) {
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		sscanf(buf, "%d %d tri %d %d %d", &itmp, &itmp, 
			&viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1], &viz_s->ie_viz[i][2]);
	};
	return IFLAG_SURFACES;
};


static void read_gz_viz_phys_data(struct psf_data *viz_s){
	int iflag, i, j, itmp, iread, nread;
	char buf[LENGTHBUF];    /* array for reading line */
	int num_word[1], nchara[1], lbuf[1];
	
	lbuf[0] = LENGTHBUF;
	
	iread = 0;
	get_one_line_from_gz(lbuf, num_word, nchara, buf);
	sscanf(buf, "%d%n", &viz_s->nfield, &nread);
	iread = iread + nread;
	
	alloc_psf_field_name_c(viz_s);
	j = 0;
	for (i = 0; i < num_word[0]-1; i++) {
		sscanf(&buf[iread], "%d%n", &viz_s->ncomp[j], &nread);
		j = j+1;
		iread = iread + nread;
	}
	
	while (j < viz_s->nfield-1) {
		iread = 0;
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		for (i = 0; i < num_word[0]; i++) {
			sscanf(&buf[iread], "%d%n", &viz_s->ncomp[j], &nread);
			j = j+1;
			iread = iread + nread;
			/*printf("ncomp: %d \n", viz_s->ncomp[i]);*/
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
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		iflag = read_field_name_from_buffer(lbuf[0], buf, viz_s->data_name[i]);
		viz_s->id_coord[i] = iflag;
/*		printf("%d, %s coordinate: %d \n", i, viz_s->data_name[i], viz_s->id_coord[i]);*/
	};
	
	/*  read field */
	
	for (i = 0; i < viz_s->nnod_viz; i++) {
		get_one_line_from_gz(lbuf, num_word, nchara, buf);
		iread = 0;
		sscanf(buf, "%d%n", &itmp, &nread); 
		iread = iread + nread;
		for (j = 0; j < viz_s->ncomptot; j++){
			sscanf(&buf[iread], "%lf%n", &viz_s->d_nod[i][j], &nread); 
			iread = iread + nread;
		};
	};
	return;
};


int read_psf_grd_gz(const char *file_head, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	int iflag_datatype;
	
	/* printf("file header in: %s \n", file_head); */
	sprintf(file_name, "%s.0.grd.gz",file_head);
	printf("gzipped grd file name: %s \n",file_name);
	iflag_datatype = open_rd_gzfile_w_flag(file_name);
	if (iflag_datatype == 1){
		return -1;
	}
	
	read_gz_viz_node_data(viz_s);
	iflag_datatype = read_gz_psf_connect_data(viz_s);
	if (iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
		return iflag_datatype;
	}
	
	close_gzfile();
	return IFLAG_SURFACES;
}

int read_psf_udt_gz(const char *file_head, int istep, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	int ierr;
	
	sprintf(file_name, "%s.%d.udt.gz",file_head,istep);
	printf("gzipped udt file name: %s \n",file_name);
	ierr = open_rd_gzfile_w_flag(file_name);
	if (ierr == 1){
		return ierr;
	}
	
	read_gz_viz_phys_data(viz_s);
	close_gzfile();
	return 0;
}

int read_kemoview_ucd_gz(const char *file_head, struct psf_data *viz_s){
	char file_name[LENGTHBUF];
	int iflag_datatype;
	
	sprintf(file_name, "%s.inp.gz",file_head);
	printf("gzipped UCD file name: %s \n",file_name);
	iflag_datatype = open_rd_gzfile_w_flag(file_name);
	if (iflag_datatype == 1) return -1;
	
	read_gz_viz_node_data(viz_s);
	iflag_datatype = read_gz_kemoview_connect_data(viz_s);
	if (iflag_datatype == -1){
		dealloc_psf_mesh_c(viz_s);
		return iflag_datatype;
	}
	
	read_gz_viz_phys_data(viz_s);
	close_gzfile();
	return iflag_datatype;
}

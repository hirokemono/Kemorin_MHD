//
//  read_psf_vtk_data_c.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 13/01/12.
//
//

#include <stdio.h>
#include "read_psf_vtk_data_c.h"

FILE *fp;

static void read_psf_vtk_node_data(struct psf_data *viz_s){
	int i;
    long offset;
	char tmpchara[8];
	char buf[LENGTHBUF];    /* array for reading line */
	
    offset = skip_comment_c(fp);
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
    int i, num_index, iflag, itmp;


	fgets(buf, LENGTHBUF, fp);     /* CELLS    nele_viz  nele*nnod_4_ele */
	sscanf(buf, "%5s %d %d", tmpchara, &viz_s->nele_viz, &num_index);
    
    viz_s->nnod_4_ele_viz = (num_index / viz_s->nele_viz) - 1;
	alloc_viz_ele_s(viz_s);
    
    if(viz_s->nele_viz == 4){
		printf("Quad patch data \n");
		iflag = IFLAG_QUAD_UCD;
        for (i = 0; i < viz_s->nele_viz; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%d %d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2], &viz_s->ie_viz[i][3]);
        };
    }
    else if(viz_s->nele_viz == 3){
		printf("Triangle patch data \n");
		iflag = IFLAG_SURF_UCD;
        for (i = 0; i < viz_s->nele_viz; i++) {
            fgets(buf, LENGTHBUF, fp);
            sscanf(buf, "%d %d %d %d", &itmp,
                   &viz_s->ie_viz[i][0], &viz_s->ie_viz[i][1],
                   &viz_s->ie_viz[i][2]);
        };
    }
    else if(viz_s->nele_viz == 2){
		printf("Line data \n");
		iflag = IFLAG_LINE_UCD;
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

    return iflag;
}

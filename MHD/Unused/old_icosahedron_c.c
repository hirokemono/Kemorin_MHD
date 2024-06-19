
long set_icosahedron_patch(double size, double x_draw[3],
                           double *xyzw_draw, double *norm_draw){
    double xyzw_plot[4*12];
	int i, j;
    int ie1;
/*    int ie2, ie3; */
    long icou, nd;
	
	for (i = 0; i < 12; i++) {
        xyzw_plot[4*i  ]=  x_draw[0] + xyzw_ico[i][0] * size;
        xyzw_plot[4*i+1]=  x_draw[1] + xyzw_ico[i][1] * size;
        xyzw_plot[4*i+2]=  x_draw[2] + xyzw_ico[i][2] * size;
        xyzw_plot[4*i+3]=  xyzw_ico[i][3];
	};
	
/* add a points to the display list */
    long num_tri = 0;
	for (i = 0; i < 20; i++){
/*
		ie1 = ifac_poi[i][0];
		ie2 = ifac_poi[i][1];
		ie3 = ifac_poi[i][2];
*/
		for (j = 0; j < 3; j++) {
            icou = 3*num_tri + j;
			for (nd = 0; nd < 4; nd++) {
				ie1 = ifac_poi[i][j];
                xyzw_draw[4*icou+nd] =  xyzw_plot[4*ie1+nd];
				norm_draw[4*icou+nd] =  xyzw_ico[ie1][nd];
			};
		};
        num_tri = num_tri + 1;
	};
	return num_tri;
}

int set_tube_vertex(int ncorner, double radius, 
                    double xyzw_line[8], double dir_line[8], 
                    double norm_line[8], double color_line[8], 
                    double *xyzw, double *norm, double *col) {
    double xyzw_w1[4*ncorner], norm_w1[4*ncorner];
    double xyzw_w2[4*ncorner], norm_w2[4*ncorner];
    int npatch_wall = 0;
    int k, nd;
    
    set_circle_of_tube(ncorner, radius, &xyzw_line[0],
                       &norm_line[0], &dir_line[0],
                       xyzw_w1, norm_w1);
    set_circle_of_tube(ncorner, radius, &xyzw_line[4],
                       &norm_line[4], &dir_line[4],
                       xyzw_w2, norm_w2);
    xyzw_line[3] = 1.0;
    xyzw_line[7] = 1.0;
    
    for(k=0;k<ncorner-1;k++){
        for(nd=0; nd<4; nd++){xyzw[4*(6*k)+  nd] = xyzw_w1[4*k+  nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(6*k+1)+nd] = xyzw_w1[4*k+4+nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(6*k+2)+nd] = xyzw_w2[4*k+4+nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k)+  nd] = norm_w1[4*k+  nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k+1)+nd] = norm_w1[4*k+4+nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k+2)+nd] = norm_w2[4*k+4+nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k)+  nd] =  color_line[  nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k+1)+nd] =  color_line[  nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k+2)+nd] =  color_line[4+nd];}
        
        for(nd=0; nd<4; nd++){xyzw[4*(6*k+3)+nd] = xyzw_w2[4*k+4+nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(6*k+4)+nd] = xyzw_w2[4*k+nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(6*k+5)+nd] = xyzw_w1[4*k+nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k+3)+nd] = norm_w2[4*k+4+nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k+4)+nd] = norm_w2[4*k+nd];}
        for(nd=0; nd<4; nd++){norm[4*(6*k+5)+nd] = norm_w1[4*k+nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k+3)+nd] = color_line[4+nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k+4)+nd] = color_line[4+nd];}
        for(nd=0; nd<4; nd++){col[4*(6*k+5)+nd] = color_line[  nd];}
    };
    
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1))+  nd] = xyzw_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1)+1)+nd] = xyzw_w1[nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1)+2)+nd] = xyzw_w2[nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1))+  nd] = norm_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1)+1)+nd] = norm_w1[nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1)+2)+nd] = norm_w2[nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1))+  nd] =  color_line[  nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1)+1)+nd] =  color_line[  nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1)+2)+nd] =  color_line[4+nd];}
    
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1)+3)+nd] = xyzw_w2[nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1)+4)+nd] = xyzw_w2[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(6*(ncorner-1)+5)+nd] = xyzw_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1)+3)+nd] = norm_w2[nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1)+4)+nd] = norm_w2[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){norm[4*(6*(ncorner-1)+5)+nd] = norm_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1)+3)+nd] =  color_line[4+nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1)+4)+nd] =  color_line[4+nd];}
    for(nd=0; nd<4; nd++){col[4*(6*(ncorner-1)+5)+nd] =  color_line[  nd];}

    npatch_wall = 2*ncorner;
	return npatch_wall;
}

int set_cone_vertex(int ncorner, double radius, 
                    double xyzw_line[8], double dir_line[8],
                    double norm_line[8], double color_line[8], 
                    double *xyzw, double *norm, double *col){
    double xyzw_w1[4*ncorner], norm_w1[4*ncorner];
    int npatch_wall = 0;
    int k, nd;
    
    set_circle_of_tube(ncorner, radius,
                       &xyzw_line[0], &norm_line[0], &dir_line[0],
                       xyzw_w1, norm_w1);
    
    for(k=0;k<ncorner-1;k++){
        for(nd=0; nd<4; nd++){xyzw[4*(3*k)+  nd] = xyzw_w1[4*k+  nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(3*k+1)+nd] = xyzw_w1[4*k+4+nd];}
        for(nd=0; nd<4; nd++){xyzw[4*(3*k+2)+nd] = xyzw_line[nd+4];}
        for(nd=0; nd<4; nd++){norm[4*(3*k)+  nd] = norm_w1[4*k+  nd];}
        for(nd=0; nd<4; nd++){norm[4*(3*k+1)+nd] = norm_w1[4*k+4+nd];}
        for(nd=0; nd<4; nd++){
            norm[4*(3*k+2)+nd] = 0.5 * (norm_w1[4*k+  nd] + norm_w1[4*k+4+nd]);
        }
    }
    
    for(nd=0; nd<4; nd++){xyzw[4*(3*(ncorner-1))+  nd] = xyzw_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(3*(ncorner-1)+1)+nd] = xyzw_w1[nd];}
    for(nd=0; nd<4; nd++){xyzw[4*(3*(ncorner-1)+2)+nd] = xyzw_line[nd+4];}
    for(nd=0; nd<4; nd++){norm[4*(3*(ncorner-1))+  nd] = norm_w1[4*(ncorner-1)+nd];}
    for(nd=0; nd<4; nd++){norm[4*(3*(ncorner-1)+1)+nd] = norm_w1[nd];}
    for(nd=0; nd<4; nd++){
        norm[4*(3*(ncorner-1)+2)+nd] = 0.5 * (norm_w1[4*(ncorner-1)+nd] + norm_w1[nd]);
    }
    
    for(k=0;k<ncorner;k++){
        for(nd=0; nd<4; nd++){col[4*(3*k)+  nd] = color_line[  nd];}
        for(nd=0; nd<4; nd++){col[4*(3*k+1)+nd] = color_line[  nd];}
        for(nd=0; nd<4; nd++){col[4*(3*k+2)+nd] = color_line[4+nd];}
    };
    
    npatch_wall = ncorner;
    return npatch_wall;
}

long set_cone_strided_buffer(const long ist_cone, int ncorner, double radius, 
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
    double norm_line[8];
    long npatch_wall;
	long k, nd;
	
    find_normal_on_line(&norm_line[0], &dir_line[0]);
    find_normal_on_line(&norm_line[4], &dir_line[4]);
    npatch_wall = set_cone_vertex(ncorner, radius,
                                  xyzw_line, dir_line,
                                  norm_line, color_line,
                                  xyzw, norm, col);
    long ist_vertex = ITHREE * npatch_wall * ist_cone;
	for (k=0; k<ITHREE*npatch_wall; k++) {
        set_node_stride_buffer((ist_vertex+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
	};
    return (ist_cone + 1);
}

long set_tube_strided_buffer(const long ist_tube, int ncorner, double radius, 
                             double xyzw_line[8], double dir_line[8], double color_line[8],
                             struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	double xyzw[4*6*ncorner], norm[4*6*ncorner], col[4*6*ncorner];
    double norm_line[8];
    long npatch_wall;
	long k, nd;
	
	find_normal_on_line(&norm_line[0], &dir_line[0]);
	find_normal_on_line(&norm_line[4], &dir_line[4]);
    npatch_wall = set_tube_vertex(ncorner, radius,
                                  xyzw_line, dir_line,
                                  norm_line, color_line,
                                  xyzw, norm, col);
    long ist_vertex = ITHREE * npatch_wall * ist_tube;
	for (k=0; k<ITHREE*npatch_wall; k++) {
        set_node_stride_buffer((ist_vertex+k), strided_buf, &point_buf);
        for(nd=0;nd<4;nd++){
            strided_buf->v_buf[nd+point_buf.igl_xyzw] = xyzw[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_norm] = norm[4*k+nd];
            strided_buf->v_buf[nd+point_buf.igl_color] = col[4*k+nd];
        };
	};
    return (ist_tube + 1);
}


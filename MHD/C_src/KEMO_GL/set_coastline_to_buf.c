
/* set_coastline_to_buf.c */

#include  "set_coastline_to_buf.h"

#define NUM_T   6
#define NUM_P   6
#define N_CURVE 180

double theta_p_grid[N_CURVE+1];
double phi_p_grid[NUM_P+1];
double theta_t_grid[NUM_T+1];
double phi_t_grid[N_CURVE+1];


void init_mapgrid_position(void){
	int i, j;
	double pi;
	
	pi = TWO * acos(ZERO);
	
	for(i=0; i<N_CURVE+1; i++){theta_p_grid[i] = (double)i * pi / (double)N_CURVE;}
	for(j=0; j<NUM_P+1; j++) {phi_p_grid[j] = (double)j * TWO * pi / (double)NUM_P;}
	
	for(j=0; j<NUM_T-1; j++){theta_t_grid[j] = (double)(j+1) * pi / (double)NUM_T;}
	for(i=0; i<N_CURVE+1; i++){phi_t_grid[i] = (double)i * TWO * pi / (double)N_CURVE;}
	return;
}

long count_sph_flame(void){
	long num_edge = (NUM_P+1) * N_CURVE + (NUM_T-1) * N_CURVE;
	return num_edge;
}

long set_sph_flame_to_buf(double radius, struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long inum;
	int i, j, nd;
	double f_color[4];
	double t_mid, p_mid;
	
	set_black_color_c(f_color);
	
	inum = 0;
	for(j=0; j<NUM_P+1; j++){
		for(i=0; i<N_CURVE; i++){
			t_mid = 0.5 * (theta_p_grid[i] + theta_p_grid[i+1]);
            set_node_stride_buffer((ITWO*inum  ), strided_buf, &point_buf);
			strided_buf->v_buf[  point_buf.igl_xyzw] = (float) (radius * sin(theta_p_grid[i]) * cos(phi_p_grid[j]));
			strided_buf->v_buf[1+point_buf.igl_xyzw] = (float) (radius * sin(theta_p_grid[i]) * sin(phi_p_grid[j]));
			strided_buf->v_buf[2+point_buf.igl_xyzw] = (float) (radius * cos(theta_p_grid[i]));
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			strided_buf->v_buf[  point_buf.igl_norm] = (float) (1.0 *    sin(theta_p_grid[i]) * cos(phi_p_grid[j]));
			strided_buf->v_buf[1+point_buf.igl_norm] = (float) (1.0 *    sin(theta_p_grid[i]) * sin(phi_p_grid[j]));
			strided_buf->v_buf[2+point_buf.igl_norm] = (float) (1.0 *    cos(theta_p_grid[i]));
            strided_buf->v_buf[3+point_buf.igl_norm] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			
            set_node_stride_buffer((ITWO*inum+1), strided_buf, &point_buf);
			strided_buf->v_buf[  point_buf.igl_xyzw] = (float) (radius * sin(t_mid) * cos(phi_p_grid[j]));
			strided_buf->v_buf[1+point_buf.igl_xyzw] = (float) (radius * sin(t_mid) * sin(phi_p_grid[j]));
			strided_buf->v_buf[2+point_buf.igl_xyzw] = (float) (radius * cos(t_mid));
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			strided_buf->v_buf[  point_buf.igl_norm] = (float) (1.0 *    sin(t_mid) * cos(phi_p_grid[j]));
			strided_buf->v_buf[1+point_buf.igl_norm] = (float) (1.0 *    sin(t_mid) * sin(phi_p_grid[j]));
			strided_buf->v_buf[2+point_buf.igl_norm] = (float) (1.0 *    cos(t_mid));
            strided_buf->v_buf[3+point_buf.igl_norm] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		for(i=0; i<N_CURVE; i++){
			p_mid = 0.5 * (phi_t_grid[i] + phi_t_grid[i+1]);
            set_node_stride_buffer((ITWO*inum  ), strided_buf, &point_buf);
			strided_buf->v_buf[  point_buf.igl_xyzw] = (float) (radius * sin(theta_t_grid[j]) * cos(phi_t_grid[i]));
			strided_buf->v_buf[1+point_buf.igl_xyzw] = (float) (radius * sin(theta_t_grid[j]) * sin(phi_t_grid[i]));
			strided_buf->v_buf[2+point_buf.igl_xyzw] = (float) (radius * cos(theta_t_grid[j]));
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			strided_buf->v_buf[  point_buf.igl_norm] = (float) (1.0 *    sin(theta_t_grid[j]) * cos(phi_t_grid[i]));
			strided_buf->v_buf[1+point_buf.igl_norm] = (float) (1.0 *    sin(theta_t_grid[j]) * sin(phi_t_grid[i]));
			strided_buf->v_buf[2+point_buf.igl_norm] = (float) (1.0 *    cos(theta_t_grid[j]));
            strided_buf->v_buf[3+point_buf.igl_norm] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			
            set_node_stride_buffer((ITWO*inum+1), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = (float) (radius * sin(theta_t_grid[j]) * cos(p_mid));
			strided_buf->v_buf[1+point_buf.igl_xyzw] = (float) (radius * sin(theta_t_grid[j]) * sin(p_mid));
			strided_buf->v_buf[2+point_buf.igl_xyzw] = (float) (radius * cos(theta_t_grid[j]));
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			strided_buf->v_buf[  point_buf.igl_norm] = (float) (1.0 *    sin(theta_t_grid[j]) * cos(p_mid));
			strided_buf->v_buf[1+point_buf.igl_norm] = (float) (1.0 *    sin(theta_t_grid[j]) * sin(p_mid));
			strided_buf->v_buf[2+point_buf.igl_norm] = (float) (1.0 *    cos(theta_t_grid[j]));
            strided_buf->v_buf[3+point_buf.igl_norm] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			inum = inum + 1;
		}
	}
	return inum;
}

long set_map_flame_to_buf(struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
    long inum;
	int i, j, nd;
	double rtp_flame[6], d_map_flame[4], f_color[4];
	
	set_black_color_c(f_color);
	
	inum = 0;
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	for(j=0; j<NUM_P+1; j++){
		rtp_flame[2] = phi_p_grid[j];
		rtp_flame[5] = phi_p_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[1] = theta_p_grid[i];
			rtp_flame[4] = theta_p_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
            set_node_stride_buffer((ITWO*inum  ), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = d_map_flame[0];
			strided_buf->v_buf[1+point_buf.igl_xyzw] = d_map_flame[1];
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.002;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			
            set_node_stride_buffer((ITWO*inum+1), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
			strided_buf->v_buf[1+point_buf.igl_xyzw] = 0.5 * (d_map_flame[1] + d_map_flame[3]);
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.002;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			inum = inum + 1;
		}
	}
	
	for(j=0; j<NUM_T-1; j++){
		rtp_flame[1] = theta_t_grid[j] ;
		rtp_flame[4] = theta_t_grid[j];
		for(i=0; i<N_CURVE; i++){
			rtp_flame[2] = phi_t_grid[i];
			rtp_flame[5] = phi_t_grid[i+1];
			
			aitoff_c(ITWO, rtp_flame, d_map_flame);
			
            set_node_stride_buffer((ITWO*inum  ), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = d_map_flame[0];
			strided_buf->v_buf[1+point_buf.igl_xyzw] = d_map_flame[1];
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.002;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			
            set_node_stride_buffer((ITWO*inum+1), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = 0.5 * (d_map_flame[0] + d_map_flame[2]);
			strided_buf->v_buf[1+point_buf.igl_xyzw] = 0.5 * (d_map_flame[1] + d_map_flame[3]);
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.002;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];}
			inum = inum + 1;
		}
	}
	return inum;
}



long count_coastline_buf(void){
	return (long) get_nedge_coastline();
};

long set_coastline_buf(double radius, struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	int iedge, k, nd;
	long nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	
	set_black_color_c(f_color);
	
	nedge_coast = get_nedge_coastline();
	for (iedge=0; iedge<nedge_coast;iedge++) {
		get_coastline(iedge, tp_coast, lake);
		
		for (k = 0; k < 2; k++) {
            set_node_stride_buffer((ITWO*iedge+k), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = (float) (radius * cos(tp_coast[2*k]) * cos(tp_coast[2*k+1]));
			strided_buf->v_buf[1+point_buf.igl_xyzw] = (float) (radius * cos(tp_coast[2*k]) * sin(tp_coast[2*k+1]));
			strided_buf->v_buf[2+point_buf.igl_xyzw] = (float) (radius * sin(tp_coast[2*k]));
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			strided_buf->v_buf[  point_buf.igl_norm] = (float) (1.0 *    cos(tp_coast[2*k]) * cos(tp_coast[2*k+1]));
			strided_buf->v_buf[1+point_buf.igl_norm] = (float) (1.0 *    cos(tp_coast[2*k]) * sin(tp_coast[2*k+1]));
			strided_buf->v_buf[2+point_buf.igl_norm] = (float) (1.0 *    sin(tp_coast[2*k]));
            strided_buf->v_buf[3+point_buf.igl_norm] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];};
		};
	};
	return nedge_coast;
}

long set_map_coastline_buf(struct gl_strided_buffer *strided_buf){
    struct gl_local_buffer_address point_buf;
	int iedge, k, nd;
	long nedge_coast;
	double tp_coast[4], lake[2], f_color[4];
	double rtp_flame[6], xy_coast[4];
	double pi;
	
	pi = TWO * acos(ZERO);
	
	set_black_color_c(f_color);
	
	rtp_flame[0] = ONE;
	rtp_flame[3] = ONE;
	nedge_coast = get_nedge_coastline();
	for (iedge = 0; iedge < nedge_coast; iedge++) {
		get_coastline(iedge, tp_coast, lake);
		for (k = 0; k < 2; k++) {
			rtp_flame[3*k+1] = -tp_coast[2*k  ]+HALF*pi;
			rtp_flame[3*k+2] =  tp_coast[2*k+1]+pi;
		};
		aitoff_c(ITWO, rtp_flame, xy_coast);
		
		for (k = 0; k < 2; k++) {
            set_node_stride_buffer((ITWO*iedge+k), strided_buf, &point_buf);
			strided_buf->v_buf[0+point_buf.igl_xyzw] = xy_coast[2*k  ];
			strided_buf->v_buf[1+point_buf.igl_xyzw] = xy_coast[2*k+1];
			strided_buf->v_buf[2+point_buf.igl_xyzw] = 0.002;
            strided_buf->v_buf[3+point_buf.igl_xyzw] = 1.0;
			for(nd=0;nd<4;nd++){strided_buf->v_buf[nd+point_buf.igl_color] = f_color[nd];};
		};
	};
	return nedge_coast;
}


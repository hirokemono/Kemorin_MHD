/*
 *  set_cube_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_cube_to_buf.h"

/*
static int num_faces = 6;
*/
static float cube_vertices [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, -1.0, 1.0}, {-1.0, -1.0, 1.0}, {-1.0, 1.0, 1.0},
	{1.0, 1.0, -1.0}, {1.0, -1.0, -1.0}, {-1.0, -1.0, -1.0}, {-1.0, 1.0, -1.0} };

static float cube_vertex_colors [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 1.0, 1.0},
	{1.0, 0.0, 1.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 1.0} };

static float cube_normals [6][3] = {
	{0.0, 0.0, -1.0}, {-1.0, 0.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0},
	{0.0, -1.0, 0.0}, {0.0, 0.0, -1.0}};
/*
static unsigned int cube_faces [6][4] = {
	{3, 2, 1, 0}, {2, 3, 7, 6}, {0, 1, 5, 4}, {3, 0, 4, 7}, {1, 2, 6, 5}, {4, 5, 6, 7} };
*/
static unsigned int cube_tri_faces [12][3] = {
			{3, 2, 1}, {3, 1, 0}, {2, 3, 7}, {2, 7, 6}, {0, 1, 5}, {0, 5, 4}, 
			{3, 0, 4}, {3, 4, 7}, {1, 2, 6}, {1, 6, 5}, {4, 5, 6}, {4, 6, 7}};

static unsigned int cube_edge [12][2] = {
			{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, 
			{6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

/*
static unsigned int cube_nodes[8] = {3, 2, 1, 0, 4, 5, 6, 7};
*/
/* cube informaiton into VBO */

struct initial_cube_buffers * init_initial_cube_buffers(void)
{
    struct initial_cube_buffers *initial_bufs = (struct initial_cube_buffers *) malloc(sizeof(struct initial_cube_buffers));
    if(initial_bufs == NULL){
        printf("malloc error in initial_cube_buffers \n");
        exit(0);
    };
    long n_point = 1024;
    initial_bufs->cube_buf =        init_strided_buffer(n_point);
    initial_bufs->cube_index_buf =  init_gl_index_buffer(12, 3);
    
    CubeNode_to_buf(0.5f, initial_bufs->cube_buf,
                    initial_bufs->cube_index_buf);
    return initial_bufs;
}

void dealloc_initial_cube_buffers(struct initial_cube_buffers *initial_bufs)
{
    dealloc_gl_index_buffer(initial_bufs->cube_index_buf);
    dealloc_strided_buffer(initial_bufs->cube_buf);
    free(initial_bufs);
};



void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf,
                     struct gl_index_buffer *index_buf){
	int i, j;
	int n_vertex = 8;
	float radius;
    
    float *x_draw;
    float *x_txur;
    float *n_draw;
    float *c_draw;
    float *d_draw;
	
	for(i=0;i<n_vertex;i++){
		x_draw = &strided_buf->v_buf[strided_buf->ist_xyz +   strided_buf->ncomp_buf*i];
		n_draw = &strided_buf->v_buf[strided_buf->ist_norm +  strided_buf->ncomp_buf*i];
		x_txur = &strided_buf->v_buf[strided_buf->ist_tex +   strided_buf->ncomp_buf*i];
		c_draw = &strided_buf->v_buf[strided_buf->ist_csurf + strided_buf->ncomp_buf*i];
        d_draw = &strided_buf->v_buf[strided_buf->ist_data +  strided_buf->ncomp_buf*i];
		
		radius = sqrt(cube_vertices[i][0]*cube_vertices[i][0]
					+ cube_vertices[i][1]*cube_vertices[i][1]
					+ cube_vertices[i][2]*cube_vertices[i][2]);
		x_draw[0] = cube_vertices[i][0] * fSize;
		x_draw[1] = cube_vertices[i][1] * fSize;
		x_draw[2] = cube_vertices[i][2] * fSize;
        x_draw[3] = 1.0;
		
		n_draw[0] = cube_vertices[i][0] / radius;
		n_draw[1] = cube_vertices[i][1] / radius;
		n_draw[2] = cube_vertices[i][2] / radius;
        n_draw[3] = 1.0;
		
		x_txur[0] = x_draw[0];
		x_txur[1] = x_draw[1];
		
		c_draw[0] = cube_vertex_colors[i][0];
		c_draw[1] = cube_vertex_colors[i][1];
		c_draw[2] = cube_vertex_colors[i][2];
		c_draw[3] = 1.0;
        
        d_draw[0] = (float) i;
	};
	
    for(i=0;i<index_buf->num_ele_buf;i++){
        for(j=0;j<index_buf->num_each_ele;j++){
            index_buf->ie_buf[i*index_buf->num_each_ele + j] = cube_tri_faces[i][j];
        };
    };
	return;
}

int flatSurfCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf){
    float *x_draw;
    float *x_txur;
    float *n_draw;
    float *c_draw;
	int i, j, k;
	
	for(j=0;j<12;j++){
		for(k=0;k<3;k++){
			x_draw = &strided_buf->v_buf[strided_buf->ist_xyz +   strided_buf->ncomp_buf*icou];
			n_draw = &strided_buf->v_buf[strided_buf->ist_norm +  strided_buf->ncomp_buf*icou];
			x_txur = &strided_buf->v_buf[strided_buf->ist_tex +   strided_buf->ncomp_buf*icou];
			c_draw = &strided_buf->v_buf[strided_buf->ist_csurf + strided_buf->ncomp_buf*icou];
		
			icou = icou + 1;
			
			i = cube_tri_faces[j][k];
			x_draw[0] = cube_vertices[i][0] * fSize;
			x_draw[1] = cube_vertices[i][1] * fSize;
			x_draw[2] = cube_vertices[i][2] * fSize;
            x_draw[3] = 1.0;
			
			n_draw[0] = cube_normals[j/2][0];
			n_draw[1] = cube_normals[j/2][1];
			n_draw[2] = cube_normals[j/2][2];
            n_draw[3] = 1.0;
		
			x_txur[0] = x_draw[0];
			x_txur[1] = x_draw[1];
		
			c_draw[0] = cube_vertex_colors[i][0];
			c_draw[1] = cube_vertex_colors[i][1];
			c_draw[2] = cube_vertex_colors[i][2];
			c_draw[3] = 1.0;
		};
	};
	
	return icou;
};

long flatEdgeCube_VBO(long icou, float fSize,
                      struct gl_strided_buffer *strided_buf,
                      struct gl_local_buffer_address *point_buf){
    float *x_draw;
    float *x_txur;
    float *n_draw;
    float *c_draw;
    float *d_draw;
	int i, j, k;
	
	for(j=0;j<12;j++){
		for(k=0;k<2;k++){
            point_buf->igl_xyzw =  strided_buf->ncomp_buf * icou + strided_buf->ist_xyz;
            point_buf->igl_color = strided_buf->ncomp_buf * icou + strided_buf->ist_csurf;
            point_buf->igl_norm =  strided_buf->ncomp_buf * icou + strided_buf->ist_norm;
            point_buf->igl_txur =  strided_buf->ncomp_buf * icou + strided_buf->ist_tex;
            point_buf->igl_data =  strided_buf->ncomp_buf * icou + strided_buf->ist_data;
            
            x_draw = &strided_buf->v_buf[point_buf->igl_xyzw];
            c_draw = &strided_buf->v_buf[point_buf->igl_color];
            n_draw = &strided_buf->v_buf[point_buf->igl_norm];
            x_txur = &strided_buf->v_buf[point_buf->igl_txur];
            d_draw = &strided_buf->v_buf[point_buf->igl_data];
			
			i = cube_edge[j][k];
			x_draw[0] = cube_vertices[i][0] * fSize;
			x_draw[1] = cube_vertices[i][1] * fSize;
			x_draw[2] = cube_vertices[i][2] * fSize;
            x_draw[3] = 1.0;
			
			n_draw[0] = cube_normals[j/2][0];
			n_draw[1] = cube_normals[j/2][1];
			n_draw[2] = cube_normals[j/2][2];
            n_draw[3] = 1.0;
		
			x_txur[0] = x_draw[0];
			x_txur[1] = x_draw[1];
		
			c_draw[0] = 0.0;
			c_draw[1] = 0.0;
			c_draw[2] = 0.0;
			c_draw[3] = 1.0;
            
            icou = icou + 1;
		};
	};
	
	return icou;
};

long flatNodeCube_VBO(long icou, float fSize, 
                      struct gl_strided_buffer *strided_buf,
                      struct gl_local_buffer_address *point_buf){
    float *x_draw;
    float *x_txur;
    float *n_draw;
    float *c_draw;
    float *d_draw;
	int i, j;
	float radius;
	
	for(j=0;j<8;j++){
        point_buf->igl_xyzw =  strided_buf->ncomp_buf * icou + strided_buf->ist_xyz;
        point_buf->igl_color = strided_buf->ncomp_buf * icou + strided_buf->ist_csurf;
        point_buf->igl_norm =  strided_buf->ncomp_buf * icou + strided_buf->ist_norm;
        point_buf->igl_txur =  strided_buf->ncomp_buf * icou + strided_buf->ist_tex;
        point_buf->igl_data =  strided_buf->ncomp_buf * icou + strided_buf->ist_data;
        
        x_draw = &strided_buf->v_buf[point_buf->igl_xyzw];
        c_draw = &strided_buf->v_buf[point_buf->igl_color];
        n_draw = &strided_buf->v_buf[point_buf->igl_norm];
        x_txur = &strided_buf->v_buf[point_buf->igl_txur];
        d_draw = &strided_buf->v_buf[point_buf->igl_data];
        
		
		i = j;
		radius = sqrt(cube_vertices[i][0]*cube_vertices[i][0]
					+ cube_vertices[i][1]*cube_vertices[i][1]
					+ cube_vertices[i][2]*cube_vertices[i][2]);
		x_draw[0] = cube_vertices[i][0] * fSize;
		x_draw[1] = cube_vertices[i][1] * fSize;
		x_draw[2] = cube_vertices[i][2] * fSize;
        x_draw[3] = 1.0;
		
		n_draw[0] = cube_normals[j/2][0];
		n_draw[1] = cube_normals[j/2][1];
		n_draw[2] = cube_normals[j/2][2];
        n_draw[3] = 1.0;
		
		x_txur[0] = x_draw[0];
		x_txur[1] = x_draw[1];
		
		c_draw[0] = 0.0;
		c_draw[1] = 0.0;
		c_draw[2] = 0.0;
		c_draw[3] = 1.0;
        
        icou = icou + 1;
	};
	
	return icou;
};


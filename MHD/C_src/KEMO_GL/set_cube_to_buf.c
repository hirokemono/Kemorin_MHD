/*
 *  set_cube_to_buf.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "set_cube_to_buf.h"

static int num_faces = 6;

static float cube_vertices [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, -1.0, 1.0}, {-1.0, -1.0, 1.0}, {-1.0, 1.0, 1.0},
	{1.0, 1.0, -1.0}, {1.0, -1.0, -1.0}, {-1.0, -1.0, -1.0}, {-1.0, 1.0, -1.0} };

static float cube_vertex_colors [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 1.0, 1.0},
	{1.0, 0.0, 1.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 1.0} };

static float cube_normals [6][3] = {
	{0.0, 0.0, -1.0}, {-1.0, 0.0, 0.0}, {1.0, 0.0, 0.0}, {0.0, 1.0, 0.0},
	{0.0, -1.0, 0.0}, {0.0, 0.0, -1.0}};

static unsigned int cube_faces [6][4] = {
	{3, 2, 1, 0}, {2, 3, 7, 6}, {0, 1, 5, 4}, {3, 0, 4, 7}, {1, 2, 6, 5}, {4, 5, 6, 7} };

static unsigned int cube_tri_faces [12][3] = {
			{3, 2, 1}, {3, 1, 0}, {2, 3, 7}, {2, 7, 6}, {0, 1, 5}, {0, 5, 4}, 
			{3, 0, 4}, {3, 4, 7}, {1, 2, 6}, {1, 6, 5}, {4, 5, 6}, {4, 6, 7}};

static unsigned int cube_edge [12][2] = {
			{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, 
			{6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

static unsigned int cube_nodes[8] = {3, 2, 1, 0, 4, 5, 6, 7};

/* cube informaiton into VBO */

struct gl_index_buffer * alloc_gl_index_buffer(int numele, int nnod_4_ele){
    struct gl_index_buffer *index_buf;
    if((index_buf = (struct gl_index_buffer *) malloc(sizeof(struct gl_index_buffer))) == NULL) {
        printf("malloc error in gl_index_buffer\n");
        exit(0);
    }
    index_buf->num_ele_buf =  numele;
    index_buf->num_each_ele = nnod_4_ele;
    index_buf->nsize_buf =    numele * nnod_4_ele;
    
    if((index_buf->ie_buf = (unsigned int *) malloc(index_buf->nsize_buf * sizeof(unsigned int))) == NULL){
        printf("malloc error in index_buf\n");
        exit(0);
    }
    return index_buf;
}

void dealloc_gl_index_buffer(struct gl_index_buffer *index_buf){
    if(index_buf->nsize_buf > 0) free(index_buf->ie_buf);
    free(index_buf);
    return;
};

void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf,
                     struct gl_index_buffer *index_buf){
	int i, j;
	int n_vertex = 8;
	float radius;
	
	for(i=0;i<n_vertex;i++){
		strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ist_xyz +   strided_buf->ncomp_buf*i];
		strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ist_norm +  strided_buf->ncomp_buf*i];
		strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ist_tex +   strided_buf->ncomp_buf*i];
		strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ist_csurf + strided_buf->ncomp_buf*i];
		
		radius = sqrt(cube_vertices[i][0]*cube_vertices[i][0]
					+ cube_vertices[i][1]*cube_vertices[i][1]
					+ cube_vertices[i][2]*cube_vertices[i][2]);
		strided_buf->x_draw[0] = cube_vertices[i][0] * fSize;
		strided_buf->x_draw[1] = cube_vertices[i][1] * fSize;
		strided_buf->x_draw[2] = cube_vertices[i][2] * fSize;
		
		strided_buf->n_draw[0] = cube_vertices[i][0] / radius;
		strided_buf->n_draw[1] = cube_vertices[i][1] / radius;
		strided_buf->n_draw[2] = cube_vertices[i][2] / radius;
		
		strided_buf->x_txur[0] = strided_buf->x_draw[0];
		strided_buf->x_txur[1] = strided_buf->x_draw[1];
		
		strided_buf->c_draw[0] = cube_vertex_colors[i][0];
		strided_buf->c_draw[1] = cube_vertex_colors[i][1];
		strided_buf->c_draw[2] = cube_vertex_colors[i][2];
		strided_buf->c_draw[3] = 1.0;
	};
	
    for(i=0;i<index_buf->num_ele_buf;i++){
        for(j=0;j<index_buf->num_each_ele;j++){
            index_buf->ie_buf[i*index_buf->num_each_ele + j] = cube_tri_faces[i][j];
        };
    };
	return;
}

int flatSurfCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf){
	int i, j, k;
	
	for(j=0;j<12;j++){
		for(k=0;k<3;k++){
			strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ist_xyz +   strided_buf->ncomp_buf*icou];
			strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ist_norm +  strided_buf->ncomp_buf*icou];
			strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ist_tex +   strided_buf->ncomp_buf*icou];
			strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ist_csurf + strided_buf->ncomp_buf*icou];
		
			icou = icou + 1;
			
			i = cube_tri_faces[j][k];
			strided_buf->x_draw[0] = cube_vertices[i][0] * fSize;
			strided_buf->x_draw[1] = cube_vertices[i][1] * fSize;
			strided_buf->x_draw[2] = cube_vertices[i][2] * fSize;
			
			strided_buf->n_draw[0] = cube_normals[j/2][0];
			strided_buf->n_draw[1] = cube_normals[j/2][1];
			strided_buf->n_draw[2] = cube_normals[j/2][2];
		
			strided_buf->x_txur[0] = strided_buf->x_draw[0];
			strided_buf->x_txur[1] = strided_buf->x_draw[1];
		
			strided_buf->c_draw[0] = cube_vertex_colors[i][0];
			strided_buf->c_draw[1] = cube_vertex_colors[i][1];
			strided_buf->c_draw[2] = cube_vertex_colors[i][2];
			strided_buf->c_draw[3] = 1.0;
		};
	};
	
	return icou;
};

int flatEdgeCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf){
	int i, j, k;
	
	for(j=0;j<12;j++){
		for(k=0;k<2;k++){
            set_node_stride_buffer(icou, strided_buf);
			icou = icou + 1;
			
			i = cube_edge[j][k];
			strided_buf->x_draw[0] = cube_vertices[i][0] * fSize;
			strided_buf->x_draw[1] = cube_vertices[i][1] * fSize;
			strided_buf->x_draw[2] = cube_vertices[i][2] * fSize;
			
			strided_buf->n_draw[0] = cube_normals[j/2][0];
			strided_buf->n_draw[1] = cube_normals[j/2][1];
			strided_buf->n_draw[2] = cube_normals[j/2][2];
		
			strided_buf->x_txur[0] = strided_buf->x_draw[0];
			strided_buf->x_txur[1] = strided_buf->x_draw[1];
		
			strided_buf->c_draw[0] = 0.0;
			strided_buf->c_draw[1] = 0.0;
			strided_buf->c_draw[2] = 0.0;
			strided_buf->c_draw[3] = 1.0;
		};
	};
	
	return icou;
};

int flatNodeCube_VBO(int icou, float fSize, struct gl_strided_buffer *strided_buf){
	int i, j;
	float radius;
	
	for(j=0;j<8;j++){
        set_node_stride_buffer(icou, strided_buf);
		icou = icou + 1;
		
		i = j;
		radius = sqrt(cube_vertices[i][0]*cube_vertices[i][0]
					+ cube_vertices[i][1]*cube_vertices[i][1]
					+ cube_vertices[i][2]*cube_vertices[i][2]);
		strided_buf->x_draw[0] = cube_vertices[i][0] * fSize;
		strided_buf->x_draw[1] = cube_vertices[i][1] * fSize;
		strided_buf->x_draw[2] = cube_vertices[i][2] * fSize;
		
		strided_buf->n_draw[0] = cube_normals[j/2][0];
		strided_buf->n_draw[1] = cube_normals[j/2][1];
		strided_buf->n_draw[2] = cube_normals[j/2][2];
		
		strided_buf->x_txur[0] = strided_buf->x_draw[0];
		strided_buf->x_txur[1] = strided_buf->x_draw[1];
		
		strided_buf->c_draw[0] = 0.0;
		strided_buf->c_draw[1] = 0.0;
		strided_buf->c_draw[2] = 0.0;
		strided_buf->c_draw[3] = 1.0;
	};
	
	return icou;
};


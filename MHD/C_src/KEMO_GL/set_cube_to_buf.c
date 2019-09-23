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

static GLuint cube_faces [6][4] = {
	{3, 2, 1, 0}, {2, 3, 7, 6}, {0, 1, 5, 4}, {3, 0, 4, 7}, {1, 2, 6, 5}, {4, 5, 6, 7} };

static GLuint cube_tri_faces [12][3] = {
			{3, 2, 1}, {3, 1, 0}, {2, 3, 7}, {2, 7, 6}, {0, 1, 5}, {0, 5, 4}, 
			{3, 0, 4}, {3, 4, 7}, {1, 2, 6}, {1, 6, 5}, {4, 5, 6}, {4, 6, 7}};

static GLuint cube_edge [12][2] = {
			{0, 1}, {1, 2}, {2, 3}, {3, 0}, {4, 5}, {5, 6}, 
			{6, 7}, {7, 4}, {0, 4}, {1, 5}, {2, 6}, {3, 7}};

static GLuint cube_nodes[8] = {3, 2, 1, 0, 4, 5, 6, 7};

/* cube informaiton into VBO */

void CubeNode_to_buf(float fSize, struct gl_strided_buffer *strided_buf){
	int i;
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
			set_node_stride_VBO(icou, strided_buf);
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
		set_node_stride_VBO(icou, strided_buf);
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

/* draw simple cube based on current modelview and projection matrices */

void cube_surf_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
	CubeNode_to_buf(fSize, gl_buf);
	
	GLenum ErrorCheckValue = glGetError();
	
	Const_VAO_4_Phong(VAO_quad, gl_buf);
	
	glBindVertexArray(VAO_quad->id_VAO);
	glDeleteBuffers(1, &VAO_quad->id_index);
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &VAO_quad->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*36, cube_tri_faces, GL_STATIC_DRAW);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);

	
	/*
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not create a VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	*/
}


void cube_edge_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
	CubeNode_to_buf(fSize, gl_buf);
	
	GLenum ErrorCheckValue = glGetError();
	
	glDeleteBuffers(1, &VAO_quad->id_vertex);
	
	glGenBuffers(1, &VAO_quad->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
				 gl_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, gl_buf->istride,
						  (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, gl_buf->istride, 
						  (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, gl_buf->istride, 
						  (GLvoid*) (gl_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glDeleteBuffers(1, &VAO_quad->id_index);
	
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &VAO_quad->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*24, cube_edge, GL_STATIC_DRAW);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	/*
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not create a VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	*/
}

void cube_flat_VBO(float fSize, struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
	int icou = 0;
	icou = flatSurfCube_VBO(icou, fSize, gl_buf);
	
	GLenum ErrorCheckValue = glGetError();
	
	glGenVertexArrays(1, &VAO_quad->id_VAO);
	glBindVertexArray(VAO_quad->id_VAO);
	
	glDeleteBuffers(1, &VAO_quad->id_vertex);

	glGenBuffers(1, &VAO_quad->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
				 gl_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, gl_buf->istride,
						  (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, gl_buf->istride, 
						  (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, gl_buf->istride, 
						  (GLvoid*) (gl_buf->ist_norm * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	/*
	ErrorCheckValue = glGetError();
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not create a VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	*/
	glBindVertexArray(0);
}


void set_quadVBO(struct VAO_ids *VAO_quad, struct gl_strided_buffer *gl_buf)
{
	float Vertices[] = {
		-0.8f,  0.8f, 0.0f, 
		0.8f,  0.8f, 0.0f, 
		-0.8f, -0.8f, 0.0f, 
		0.8f, -0.8f, 0.0f
	};
	
	float Colors[] = {
		1.0f, 0.0f, 0.0f, 1.0f,
		0.0f, 1.0f, 0.0f, 1.0f,
		0.0f, 0.0f, 1.0f, 1.0f,
		1.0f, 1.0f, 1.0f, 1.0f
	};
	
	GLuint quad_tri_faces [2][3] = {{0, 2, 1},  {2, 3, 1}};
	
	int i, nd;
	
	for(i=0;i<gl_buf->num_nod_buf;i++){
		for(nd=0;nd<3;nd++){
			gl_buf->v_buf[nd + gl_buf->ist_xyz + i*gl_buf->ncomp_buf] = Vertices[nd+3*i];
		};
		for(nd=0;nd<4;nd++){
			gl_buf->v_buf[nd + gl_buf->ist_csurf + i*gl_buf->ncomp_buf] = Colors[nd+4*i];
		};
	};
	
	GLenum ErrorCheckValue = glGetError();
	
	glDeleteBuffers(1, &VAO_quad->id_vertex);

	glGenBuffers(1, &VAO_quad->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, VAO_quad->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(float) * gl_buf->num_nod_buf*gl_buf->ncomp_buf,
				 gl_buf->v_buf, GL_STATIC_DRAW);
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, gl_buf->istride,
						  (GLvoid*) (gl_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, gl_buf->istride, 
						  (GLvoid*) (gl_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	
	glDeleteBuffers(1, &VAO_quad->id_index);
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &VAO_quad->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, VAO_quad->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(GLuint)*6, quad_tri_faces, GL_STATIC_DRAW);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	ErrorCheckValue = glGetError();
	/*
	if (ErrorCheckValue != GL_NO_ERROR)
	{
		fprintf(
				stderr,
				"ERROR: Could not create a VBO: %s \n",
				gluErrorString(ErrorCheckValue)
				);
		
		exit(-1);
	}
	 */
}

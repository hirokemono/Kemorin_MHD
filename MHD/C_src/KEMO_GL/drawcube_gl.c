/*
 *  drawcube_gl.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "drawcube_gl.h"

static GLint num_faces = 6;

static GLfloat cube_vertices [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, -1.0, 1.0}, {-1.0, -1.0, 1.0}, {-1.0, 1.0, 1.0},
	{1.0, 1.0, -1.0}, {1.0, -1.0, -1.0}, {-1.0, -1.0, -1.0}, {-1.0, 1.0, -1.0} };

static GLfloat cube_vertex_colors [8][3] = {
	{1.0, 1.0, 1.0}, {1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 1.0, 1.0},
	{1.0, 0.0, 1.0}, {1.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 1.0} };

static GLfloat cube_normals [6][3] = {
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

int flatSurfCube_VBO(int icou, GLfloat fSize, struct gl_strided_buffer *strided_buf){
	int i, j, k;
	
	for(j=0;j<12;j++){
		for(k=0;k<3;k++){
			set_node_stride_VBO(icou, strided_buf);
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

int flatEdgeCube_VBO(int icou, GLfloat fSize, struct gl_strided_buffer *strided_buf){
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

int flatNodeCube_VBO(int icou, GLfloat fSize, struct gl_strided_buffer *strided_buf){
	int i, j, k;
	GLfloat radius;
	
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
void drawCube(GLfloat fSize)
{
	long f, i;
	GLfloat c_code[4][4];
	GLfloat x_draw[4][3];
	GLfloat x_norm[4][3];

	glBegin (GL_QUADS);
	
	for (f = 0; f < num_faces; f++){
		for (i = 0; i < 4; i++) {
			c_code[i][0] = cube_vertex_colors[cube_faces[f][i]][0];
			c_code[i][1] = cube_vertex_colors[cube_faces[f][i]][1];
			c_code[i][2] = cube_vertex_colors[cube_faces[f][i]][2];
			x_draw[i][0] = cube_vertices[cube_faces[f][i]][0] * fSize;
			x_draw[i][1] = cube_vertices[cube_faces[f][i]][1] * fSize;
			x_draw[i][2] = cube_vertices[cube_faces[f][i]][2] * fSize;
 			x_norm[i][0] = cube_normals[f][0];
			x_norm[i][1] = cube_normals[f][1];
			x_norm[i][2] = cube_normals[f][2];
		};
		for (i = 0; i < 4; i++) {
			glColor4fv(&c_code[i][0]);
			glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE, c_code[i]);
			glNormal3f(x_norm[i][0], x_norm[i][1], x_norm[i][2]);
			glVertex3f(x_draw[i][0], x_draw[i][1], x_draw[i][2]);
		};
	};

	glEnd ();

	c_code[0][0] = 0;
	c_code[0][1] = 0;
	c_code[0][2] = 0;
	c_code[0][3] = 1.0;

	glColor4fv(&c_code[0][0]);

	for (f = 0; f < num_faces; f++) {
		for (i = 0; i < 4; i++) {
			x_draw[i][0] = cube_vertices[cube_faces[f][i]][0] * fSize;
			x_draw[i][1] = cube_vertices[cube_faces[f][i]][1] * fSize;
			x_draw[i][2] = cube_vertices[cube_faces[f][i]][2] * fSize;
		};
		glBegin (GL_LINE_LOOP);
		for (i = 0; i < 4; i++)
			glVertex3f(x_draw[i][0], x_draw[i][1], x_draw[i][2]);
		glEnd ();
	}

	return;
}

void drawCube_array(GLfloat fSize)
{
	long f, i;
	GLfloat x_draw[12];
	GLfloat x_norm[12];
	GLfloat c_code[12];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	/*
	glEnableClientState(GL_INDEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	 */

	glVertexPointer(ITHREE, GL_FLOAT, IZERO, x_draw);
	glNormalPointer(GL_FLOAT, IZERO, x_norm);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, c_code);
	for (f = 0; f < num_faces; f++){
		for (i = 0; i < 4; i++) {
			c_code[4*i  ] = cube_vertex_colors[cube_faces[f][i]][0];
			c_code[4*i+1] = cube_vertex_colors[cube_faces[f][i]][1];
			c_code[4*i+2] = cube_vertex_colors[cube_faces[f][i]][2];
			c_code[4*i+3] = 1.0;
			x_draw[3*i  ] = cube_vertices[cube_faces[f][i]][0] * fSize;
			x_draw[3*i+1] = cube_vertices[cube_faces[f][i]][1] * fSize;
			x_draw[3*i+2] = cube_vertices[cube_faces[f][i]][2] * fSize;

			x_norm[3*i  ] = cube_normals[f][0];
			x_norm[3*i+1] = cube_normals[f][1];
			x_norm[3*i+2] = cube_normals[f][2];
/*
			x_norm[3*i  ] = x_draw[3*i  ];
			x_norm[3*i+1] = x_draw[3*i+1];
			x_norm[3*i+2] = x_draw[3*i+2];
*/
		};
		glDrawArrays(GL_QUADS, IZERO, IFOUR);
	};

	for (f = 0; f < num_faces; f++) {
		for (i = 0; i < 4; i++) {
			x_draw[3*i  ] = cube_vertices[cube_faces[f][i]][0] * fSize;
			x_draw[3*i+1] = cube_vertices[cube_faces[f][i]][1] * fSize;
			x_draw[3*i+2] = cube_vertices[cube_faces[f][i]][2] * fSize;
			c_code[3*i  ] = 0.;
			c_code[3*i+1] = 0.;
			c_code[3*i+2] = 0.;
			c_code[4*i+3] = 1.0;
		};

		glVertexPointer(ITHREE, GL_FLOAT, IZERO, x_draw);
		glColorPointer(IFOUR, GL_FLOAT, IZERO, c_code);
		glDrawArrays(GL_LINE_LOOP, IZERO, IFOUR);
	}

	/*
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_INDEX_ARRAY);
	*/
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;
}


void drawCube_Element2(GLfloat fSize, 
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO){
	int i;
	int n_vertex = 8;
	strided_buf->ncomp_buf = 20;
	
	strided_buf->ist_xyz =  0;
	strided_buf->ist_norm = 3;
	strided_buf->ist_tex =  6;
	strided_buf->ist_csurf = 8;
	int ist_cedge = 12;
	int ist_cnode = 16;
	
	GLfloat *c_edge;
	GLfloat *c_dots;
	
	GLfloat radius;
	
	/* Set Stride for each vertex buffer */
	GLsizei stride = sizeof(GLfloat) * strided_buf->ncomp_buf;

	for(i=0;i<n_vertex;i++){
		strided_buf->x_draw = &strided_buf->v_buf[strided_buf->ist_xyz +   strided_buf->ncomp_buf*i];
		strided_buf->n_draw = &strided_buf->v_buf[strided_buf->ist_norm +  strided_buf->ncomp_buf*i];
		strided_buf->x_txur = &strided_buf->v_buf[strided_buf->ist_tex +   strided_buf->ncomp_buf*i];
		strided_buf->c_draw = &strided_buf->v_buf[strided_buf->ist_csurf + strided_buf->ncomp_buf*i];
		c_edge = &strided_buf->v_buf[ist_cedge + strided_buf->ncomp_buf*i];
		c_dots = &strided_buf->v_buf[ist_cnode + strided_buf->ncomp_buf*i];
		
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
		
		c_edge[0] = 0.0;
		c_edge[1] = 0.0;
		c_edge[2] = 0.0;
		c_edge[3] = 1.0;
		
		c_dots[0] = 1.0;
		c_dots[1] = 1.0;
		c_dots[2] = 1.0;
		c_dots[3] = 1.0;
	};
		
	/* Create vertex buffer on GPU and cpoy data from CPU*/
	glGenBuffers(1, &cube_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cube_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * n_vertex*strided_buf->ncomp_buf,
				strided_buf->v_buf, GL_STATIC_DRAW);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	/* Set Vertex buffer */
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	/*
	glEnableClientState(GL_INDEX_ARRAY);
	*/
	
	/* Stride is size of buffer for each point */
	glBindBuffer(GL_ARRAY_BUFFER, cube_VAO->id_vertex);
	glVertexPointer(3,   GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_xyz));
	glNormalPointer(     GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_norm));
	glTexCoordPointer(2, GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_tex));
	glColorPointer(4,    GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_csurf));
	
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &cube_VAO->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*36, cube_tri_faces, GL_STATIC_DRAW);
	
	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &cube_VAO->id_index);
	

	
	glBindBuffer(GL_ARRAY_BUFFER, cube_VAO->id_vertex);
	glVertexPointer(3,   GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_xyz));
	glNormalPointer(     GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_norm));
	glTexCoordPointer(2, GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_tex));
	glColorPointer(4,    GL_FLOAT, stride, (sizeof(GLfloat)*ist_cedge));
	
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &cube_VAO->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*24, cube_edge, GL_STATIC_DRAW);
	
	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, 0);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &cube_VAO->id_index);

	glBindBuffer(GL_ARRAY_BUFFER, cube_VAO->id_vertex);
	glVertexPointer(3,   GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_xyz));
	glNormalPointer(     GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_norm));
	glTexCoordPointer(2, GL_FLOAT, stride, (sizeof(GLfloat)*strided_buf->ist_tex));
	glColorPointer(4,    GL_FLOAT, stride, (sizeof(GLfloat)*ist_cnode));
	
	/* Create index buffer on GPU, and then copy from CPU */
	glGenBuffers(1, &cube_VAO->id_index);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_VAO->id_index);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(int)*8, cube_nodes, GL_STATIC_DRAW);
	
	glDrawElements(GL_POINTS, 8, GL_UNSIGNED_INT, 0);
	
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	glDeleteBuffers(1, &cube_VAO->id_index);

	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	
	return;
}


void drawCube_flat(GLfloat fSize, 
			struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO){
	int i, j, k, icou;
	GLfloat radius;
	
	/* Set Stride for each vertex buffer */
	set_buffer_address_4_patch(strided_buf);
	strided_buf->istride = strided_buf->ncomp_buf;
	
	icou = 0;
	icou = flatSurfCube_VBO(icou, fSize, strided_buf);
	icou = flatEdgeCube_VBO(icou, fSize, strided_buf);
	icou = flatNodeCube_VBO(icou, fSize, strided_buf);
	
	/* Create vertex buffer on GPU and cpoy data from CPU*/
	glGenBuffers(1, &cube_VAO->id_vertex);
	glBindBuffer(GL_ARRAY_BUFFER, cube_VAO->id_vertex);
	glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat) * (8+24+36)*strided_buf->ncomp_buf,
				strided_buf->v_buf, GL_STATIC_DRAW);
	
	/* Set Vertex buffer */
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	/*
	glEnableClientState(GL_INDEX_ARRAY);
	*/
	
	/* Stride is size of buffer for each point */
	glVertexPointer(3,   GL_FLOAT, (sizeof(GLfloat)*strided_buf->istride),
				(sizeof(GLfloat)*strided_buf->ist_xyz));
	glNormalPointer(     GL_FLOAT, (sizeof(GLfloat)*strided_buf->istride),
				(sizeof(GLfloat)*strided_buf->ist_norm));
	glTexCoordPointer(2, GL_FLOAT, (sizeof(GLfloat)*strided_buf->istride),
				(sizeof(GLfloat)*strided_buf->ist_tex));
	glColorPointer(4,    GL_FLOAT, (sizeof(GLfloat)*strided_buf->istride),
				(sizeof(GLfloat)*strided_buf->ist_csurf));
	
	
	glDrawArrays(GL_TRIANGLES, 0,  36);
	glDrawArrays(GL_LINES,     36, 24);
	glDrawArrays(GL_POINTS,    60,  8);
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	
	return;
}


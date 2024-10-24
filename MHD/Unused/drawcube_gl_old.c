/*
 *  drawcube_gl_old.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/02/24.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "drawcube_gl_old.h"

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

/* draw simple cube based on current modelview and projection matrices */

void drawCube(GLfloat fSize)
{
	long f, i;
	GLfloat c_code[4][4];
	GLfloat x_draw[4][3];
	GLfloat x_norm[4][3];

	glEnable(GL_CULL_FACE);
	glEnable(GL_COLOR_MATERIAL);
	glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
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
	glDisable(GL_COLOR_MATERIAL);

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

	glEnable(GL_CULL_FACE);
	
	glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);

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
	
	glDisable(GL_COLOR_MATERIAL);

	/*
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_INDEX_ARRAY);
	*/
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;
}

void drawCube_Element(GLfloat fSize)
{
	long f, i;
	GLfloat x_draw[24];
	GLfloat x_norm[36];
	GLfloat c_code[32];
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	/*
	glEnableClientState(GL_INDEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	*/
	
	for (i = 0; i < 8; i++) {
		c_code[4*i  ] = cube_vertex_colors[i][0];
		c_code[4*i+1] = cube_vertex_colors[i][1];
		c_code[4*i+2] = cube_vertex_colors[i][2];
		c_code[4*i+3] = 1.0;
	}
	for (i = 0; i < 8; i++) {
		x_draw[3*i  ] = cube_vertices[i][0] * fSize;
		x_draw[3*i+1] = cube_vertices[i][1] * fSize;
		x_draw[3*i+2] = cube_vertices[i][2] * fSize;
	};
	for (f = 0; f < num_faces; f++){
		x_norm[6*f  ] = cube_normals[f][0];
		x_norm[6*f+1] = cube_normals[f][1];
		x_norm[6*f+2] = cube_normals[f][2];
		x_norm[6*f+3] = cube_normals[f][0];
		x_norm[6*f+4] = cube_normals[f][1];
		x_norm[6*f+5] = cube_normals[f][2];
	};
	glVertexPointer(ITHREE, GL_FLOAT, IZERO, x_draw);
	glColorPointer(IFOUR, GL_FLOAT, IZERO, c_code);
	glNormalPointer(GL_FLOAT, IZERO, x_norm);
	
	glEnable(GL_CULL_FACE);
	
	glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);
	glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, &cube_tri_faces[0][0]);
	
	for (i = 0; i < 8; i++) {
		c_code[4*i  ] = 0.;
		c_code[4*i+1] = 0.;
		c_code[4*i+2] = 0.;
		c_code[4*i+3] = 1.0;
	};
	
	for (f = 0; f < num_faces; f++) {
		for (i = 0; i < 8; i++) {
			x_norm[3*i  ] = cube_normals[f][0];
			x_norm[3*i+1] = cube_normals[f][1];
			x_norm[3*i+2] = cube_normals[f][2];
		};
	}
	glDrawElements(GL_LINES, 24, GL_UNSIGNED_INT, &cube_edge[0][0]);
	
	for (i = 0; i < 8; i++) {
		c_code[4*i  ] = 1.0;
		c_code[4*i+1] = 1.0;
		c_code[4*i+2] = 1.0;
		c_code[4*i+3] = 1.0;
	};
	glDrawElements(GL_POINTS, 8, GL_UNSIGNED_INT, &cube_nodes[0]);
	
	glDisable(GL_COLOR_MATERIAL);

	/*
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_INDEX_ARRAY);
	*/
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	
	return;
}

void drawCube_flat(GLfloat fSize, 
				   struct gl_strided_buffer *strided_buf, struct VAO_ids *cube_VAO){
	int icou;
	
	/* Set Stride for each vertex buffer */
	set_buffer_address_4_patch(8, strided_buf);
	strided_buf->istride = sizeof(GLfloat)*strided_buf->ncomp_buf;
	
	icou = 0;
	icou = flatSurfCube_VBO(icou, fSize, strided_buf);
	icou = flatEdgeCube_VBO(icou, fSize, strided_buf);
	icou = flatNodeCube_VBO(icou, fSize, strided_buf);
	
	/* Create VAO */
	glGenVertexArrays(1, &cube_VAO->id_VAO);
	glBindVertexArray(cube_VAO->id_VAO);
	
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
	glVertexPointer(3,   GL_FLOAT, strided_buf->istride,
					(sizeof(GLfloat)*strided_buf->ist_xyz));
	glNormalPointer(     GL_FLOAT, strided_buf->istride,
					(sizeof(GLfloat)*strided_buf->ist_norm));
	glTexCoordPointer(2, GL_FLOAT, strided_buf->istride,
					  (sizeof(GLfloat)*strided_buf->ist_tex));
	glColorPointer(4,    GL_FLOAT, strided_buf->istride,
				   (sizeof(GLfloat)*strided_buf->ist_csurf));
	
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, strided_buf->istride,
						  (GLvoid*) (strided_buf->ist_xyz * sizeof(GL_FLOAT)));
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_norm * sizeof(GL_FLOAT)));
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_tex * sizeof(GL_FLOAT)));
	glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, strided_buf->istride, 
						  (GLvoid*) (strided_buf->ist_csurf * sizeof(GL_FLOAT)));
	
	glEnableVertexAttribArray(0);
	glEnableVertexAttribArray(1);
	glEnableVertexAttribArray(2);
	glEnableVertexAttribArray(3);
	
	
	glDrawArrays(GL_TRIANGLES, 0,  36);
	glDrawArrays(GL_LINES,     36, 24);
	glDrawArrays(GL_POINTS,    60,  8);
	
	
	glDisableVertexAttribArray(3);
	glDisableVertexAttribArray(2);
	glDisableVertexAttribArray(1);
	glDisableVertexAttribArray(0);
	
	glBindBuffer(GL_ARRAY_BUFFER, 0);
	
	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	
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
	
	CubeNode_to_buf(fSize, strided_buf);
	
	for(i=0;i<n_vertex;i++){
		c_edge = &strided_buf->v_buf[ist_cedge + strided_buf->ncomp_buf*i];
		c_dots = &strided_buf->v_buf[ist_cnode + strided_buf->ncomp_buf*i];
		
		c_edge[0] = 0.0;
		c_edge[1] = 0.0;
		c_edge[2] = 0.0;
		c_edge[3] = 1.0;
		
		c_dots[0] = 1.0;
		c_dots[1] = 1.0;
		c_dots[2] = 1.0;
		c_dots[3] = 1.0;
	};
	
	
	/* Create VAO */
	glGenVertexArrays(1, &cube_VAO->id_VAO);
	glBindVertexArray(cube_VAO->id_VAO);
	
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




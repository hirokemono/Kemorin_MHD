/*
 *  write_modelview_matrix.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/11.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "write_modelview_matrix.h"

FILE *fp_mat;

void output_GL_modelview_matrix(FILE *fp, struct view_element *view) {
	int i, j;
	
	glGetDoublev(GL_MODELVIEW_MATRIX, view->mat_object_2_eye);
	
	fprintf(fp, "    array modelview_matrix_ctl    16\n");
	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			fprintf(fp, "      modelview_matrix_ctl   %d  %d   %.12e \n",
					(i+1), (j+1), view->mat_object_2_eye[i+4*j]);
		};
		fprintf(fp, "!\n");
	};
	fprintf(fp, "    end array modelview_matrix_ctl\n");
	fprintf(fp, "!\n");
	
	return;
}


void output_GL_projection_matrix(FILE *fp, struct view_element *view) {
	int i, j;
	
	glGetDoublev(GL_PROJECTION_MATRIX, view->mat_eye_2_clip);
	
	fprintf(fp, "    begin projection_matrix_ctl\n");
	for (j = 0; j < 4; j++) {
		for (i = 0; i < 4; i++) {
			fprintf(fp, "      matrix44_comp_ctl   %d  %d   %.12e \n",
					(i+1), (j+1), view->mat_eye_2_clip[i+4*j]);
		};
		fprintf(fp, "!\n");
	};
	fprintf(fp, "    end projection_matrix_ctl\n");
	fprintf(fp, "!\n");
	
	return;
}

void output_stereo_parameter(FILE *fp, struct view_element *view) {
	fprintf(fp, "    begin streo_view_parameter_ctl\n");
	fprintf(fp, "      focal_point_ctl            %.12e end\n", view->focal_length);
	fprintf(fp, "      eye_separation_ctl         %.12e end\n", view->eye_separation);
	fprintf(fp, "    end streo_view_parameter_ctl\n");
    
	return;
}


void output_GL_modelview_parameters(FILE *fp, struct view_element *view) {
	int i;
	double viewpt_in_view[3];
	double lookat_in_view[3];
	
	for (i = 0; i < 3; i++) viewpt_in_view[i] = -view->shift[i];
	for (i = 0; i < 3; i++) lookat_in_view[i] = -view->shift[i];
	lookat_in_view[2] = view->x_lookat[2];
	
	fprintf(fp, "    begin image_size_ctl\n");
	fprintf(fp, "      x_pixel_ctl   %d\n", view->nx_window);
	fprintf(fp, "      y_pixel_ctl   %d\n", view->ny_window);
	fprintf(fp, "    end image_size_ctl\n");
	fprintf(fp, "!\n");
	
	
	fprintf(fp, "    array viewpoint_in_viewer_ctl  3\n");
	fprintf(fp, "      viewpoint_in_viewer_ctl   x   %.12e \n",viewpt_in_view[0]);
	fprintf(fp, "      viewpoint_in_viewer_ctl   y   %.12e \n",viewpt_in_view[1]);
	fprintf(fp, "      viewpoint_in_viewer_ctl   z   %.12e \n",viewpt_in_view[2]);
	fprintf(fp, "    end array viewpoint_in_viewer_ctl\n");
	fprintf(fp, "!\n");
	
	fprintf(fp, "    scale_factor_ctl      %.12e\n", view->iso_scale);
	fprintf(fp, "!\n");
	
	fprintf(fp, "    array look_at_point_ctl  3\n");
	fprintf(fp, "      look_at_point_ctl         x   %.12e \n", lookat_in_view[0]);
	fprintf(fp, "      look_at_point_ctl         y   %.12e \n", lookat_in_view[1]);
	fprintf(fp, "      look_at_point_ctl         z   %.12e \n", lookat_in_view[2]);
	fprintf(fp, "    end array look_at_point_ctl\n");
	fprintf(fp, "!\n");
	
	fprintf(fp, "    array view_rotation_vec_ctl  3\n");
	fprintf(fp, "      view_rotation_vec_ctl     x   %.12e \n", view->rotation[1]);
	fprintf(fp, "      view_rotation_vec_ctl     y   %.12e \n", view->rotation[2]);
	fprintf(fp, "      view_rotation_vec_ctl     z   %.12e \n", view->rotation[3]);
	fprintf(fp, "    end array view_rotation_vec_ctl\n");
	fprintf(fp, "    view_rotation_deg_ctl   %.12e \n", view->rotation[0]);
	fprintf(fp, "!\n");
	
	fprintf(fp, "    begin projection_matrix_ctl\n");
	fprintf(fp, "      perspective_angle_ctl      %.12e \n", view->aperture);
	fprintf(fp, "      perspective_xy_ratio_ctl   %.12e \n", view->aspect);
	fprintf(fp, "      perspective_near_ctl       %.12e \n", view->near);
	fprintf(fp, "      perspective_far_ctl        %.12e \n", view->far);
	fprintf(fp, "    end projection_matrix_ctl\n");
	fprintf(fp, "!\n");
	
	return;
}


void input_GL_modelview_matrix(FILE *fp, struct view_element *view) {
	int i, j, k;
    long offset;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	char ctmp[32], ctmp1[32], ctmp2[32];   /* character buffer for reading line */
    
    offset = skip_comment_c(fp);
	for (k = 0; k < 12; k++) {
        offset = skip_comment_c(fp);
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%s %s %s %s", ctmp, ctmp1, ctmp2, ctmp);
        i = atoi(ctmp1);
        j = atoi(ctmp2);
        view->mat_object_2_eye[i+4*(j-1)] = atof(ctmp);
	};
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
	
	return;
}

void input_GL_projection_matrix(FILE *fp, struct view_element *view) {
	int i, j, k;
    long offset;
	char buf[LENGTHBUF];            /* character buffer for reading line */
	char ctmp[32], ctmp1[32], ctmp2[32];   /* character buffer for reading line */
    
    offset = skip_comment_c(fp);
	for (k = 0; k < 12; k++) {
        offset = skip_comment_c(fp);
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%s %s %s %s", ctmp, ctmp1, ctmp2, ctmp);
        i = atoi(ctmp1);
        j = atoi(ctmp2);
        view->mat_eye_2_clip[i+4*(j-1)] = atof(ctmp);
	};
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
	
	return;
}


void input_stereo_parameter(FILE *fp, struct view_element *view) {
    long offset;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
    
    
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp);
    view->focal_length = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp);
    view->eye_separation = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    
	return;
}

void input_GL_modelview_parameters(FILE *fp, struct view_element *view) {
	int i, k;
	double viewpt_in_view[3];
	double lookat_in_view[3];
    
    long offset;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
    
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp);
    view->nx_window = atoi(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp);
    view->ny_window = atoi(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
	
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    for (i=0; i<3; i++) {
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%s %s %s ", ctmp, ctmp, ctmp);
        viewpt_in_view[i] = atof(ctmp);
    };
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->iso_scale = atof(ctmp);
    
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    for (i=0; i<3; i++) {
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%s %s %s ", ctmp, ctmp, ctmp);
        lookat_in_view[i] = atof(ctmp);
    };
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    for (k=0; k<3; k++) {
        fgets(buf, LENGTHBUF, fp);
        sscanf(buf, "%s %s %s ", ctmp, ctmp, ctmp);
        view->rotation[i+1] = atof(ctmp);
    };
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->rotation[0] = atof(ctmp);
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->aperture = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->aspect = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->near = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s ", ctmp, ctmp);
    view->far = atof(ctmp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp);
	
	for (i = 0; i < 3; i++) view->shift[i] = -viewpt_in_view[i];
	view->x_lookat[2] = lookat_in_view[2];
	return;
}


void write_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view){
    
	printf("ViewMatrix file name: %s \n",file_name);
	if ((fp_mat = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	}
    
	fprintf(fp_mat, "  begin view_transform_ctl\n");
	fprintf(fp_mat, "!\n");
    
    /*
    output_GL_modelview_matrix(fp_mat, view);
    output_GL_projection_matrix(fp_mat, view);
    */
    
    output_GL_modelview_parameters(fp_mat, view);
	if(iflag_view == VIEW_STEREO) output_stereo_parameter(fp_mat, view);
    
	fprintf(fp_mat, "  end view_transform_ctl\n");
	fprintf(fp_mat, "!\n");
    
	fclose(fp_mat);
	return;
}


void read_GL_modelview_file(const char *file_name, int iflag_view, struct view_element *view){
	long  offset;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
    
	printf("ViewMatrix file name: %s \n",file_name);
	if ((fp_mat = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	}
    
    offset = skip_comment_c(fp_mat);
    fgets(buf, LENGTHBUF, fp_mat);
    sscanf(buf, "%s",  ctmp);
    
	input_GL_modelview_parameters(fp_mat, view);
	if(iflag_view == VIEW_STEREO) input_stereo_parameter(fp_mat, view);
    
    fgets(buf, LENGTHBUF, fp_mat);
    sscanf(buf, "%s",  ctmp);
    
	fclose(fp_mat);
	return;
}

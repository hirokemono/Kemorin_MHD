/*
 *  kemoview_glut_console_input.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_glut_console_input.h"

/* Routines for inout from console */

void input_file_name(char *file_name){
	char buf[LENGTHBUF];
	char *delchara;
	
	printf("Input file name for Kemoviewer\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_name, buf);
}

void input_file_header(char *file_head){
	char buf[LENGTHBUF];
	char *delchara;
	
	printf("Input file header for images\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_head, buf);
}

void set_pickup_command(char *file_name){
	char buf[LENGTHBUF];
	char *delchara;
	
	sprintf(file_name,"%s","pick_surface");
	printf("Input pickup surface command\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_name, buf);
}

int input_image_format(){
	char buf[LENGTHBUF];
	char image_fmt[LENGTHBUF];
	char *delchara;
	int id_img;
	
	printf("Input Image format\n");
	printf("	 0:  NO: No Image\n");
	printf("	 1: PNG: PNG Images\n");
	printf("	 2: BMP: Bitmap Images\n");
	printf("	10: EPS: Encapsulated PostScript\n");
	printf("	11:  PS: PostScript\n");
	printf("	20: PDF:  Portable Document Format\n");
	fgets(buf,sizeof(buf),stdin);
	
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(image_fmt, buf);
	
	id_img = set_image_file_format_id(image_fmt);
	return id_img;
}


void read_kemoview_data_glut(){
	char file_name[LENGTHBUF];
	char file_head[LENGTHBUF];
	char file_head2[LENGTHBUF];
	char file_ext[LENGTHBUF];
	char pick_command[LENGTHBUF];
	int iflag_datatype;
	
	input_file_name(file_name);
	get_ext_from_file_name(file_name, file_head, file_ext);
	printf("file name: %s\n", file_name);
	printf("file_head %s\n", file_head);
	printf("file_ext %s\n", file_ext);
	
	if (		  (file_ext[0] == 'g' && file_ext[1] == 'z')
		||	  (file_ext[0] == 'G' && file_ext[1] == 'Z') ){
		get_ext_from_file_name(file_head, file_head2, file_ext);
		
		if (file_ext[0] == '0' && file_ext[1] == '\0') {
			return;
		}
	} else if (file_ext[0] == '0' && file_ext[1] == '\0') {
		set_pickup_command(pick_command);
		set_to_pick_surface_command(pick_command);
	}
	
	iflag_datatype = kemoview_open_data_glut(file_name);
	return;
};

/* Routines for values from console input */

static void input_range_from_console(char *name, float *range_min, float *range_max,
							  double data_min, double data_max)
{
	char buf[1024];
	printf("Input minimum value and maximum value. \n");
	printf("min. and max. of %s \n",name);
	printf("minimum value: %.7e \n", data_min);
	printf("maximum value: %.7e \n", data_max);
	printf("Current minimum range: %.7e \n", *range_min);
	printf("Current maximum range  %.7e \n", *range_max);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f %f", range_min, range_max);
	printf("modified minimum range: %.7e \n", *range_min);
	printf("modified maximum range  %.7e \n", *range_max);
	return;
}

static float set_opacity_console(float opacity_org){
	char buf[1024];
	float opacity_input;
	
	printf("Current opacity_org  %.7e \n", opacity_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &opacity_input);
	
	if ( opacity_input < ZERO) opacity_input = ZERO;
	if ( opacity_input > ONE)  opacity_input =  ONE;
	printf("modified Opacity: %.7e \n", opacity_input);
	
	return opacity_input;
}

void set_psf_range_console(){
	float range_min, range_max;
	char name[1024];
	
	int ifield = send_draw_field_current_psf();
	int icomp = send_draw_component_current_psf();
	float data_min = (float) send_current_psf_data_min(icomp);
	float data_max = (float) send_current_psf_data_max(icomp);
	send_current_psf_data_name(name, ifield);
	
	input_range_from_console(name, &range_min, &range_max, data_min, data_max);
	set_current_PSF_linear_colormap((double) range_min, (double) range_max);
	return;
}

void set_fline_range_console(){
	float range_min, range_max;
	char name[1024];
	
	int ifield = send_if_draw_fline();
	int icomp = send_icomp_draw_fline();
	float data_min = (float) send_fline_data_min(icomp);
	float data_max = (float) send_fline_data_max(icomp);
	send_fline_data_name(name, ifield);
	
	input_range_from_console(name, &range_min, &range_max, data_min, data_max);	
	input_fline_linear_colormap((double) range_min, (double) range_max);
	return;
}

void set_fline_thick_console(){
	float thick;
	char buf[1024];
	
	thick = (float) send_fline_thickness();
	printf("Input fieldline thickness. \n");
	printf("Current thickness: %e \n", thick);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%e ", &thick);
	printf("modified  number of line: %e \n", thick);
	
	if(thick > 0) set_to_fline_thickness((double) thick);
	return;
}

void set_num_isoline(){
	int nline;
	char buf[1024];
	
	nline = send_current_num_isoline();
	printf("Input number of isolines. \n");
	printf("Current number of line: %d \n", nline);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d ", &nline);
	printf("modified  number of line: %d \n", nline);
	
	if(nline > 0) set_current_n_isoline(nline);
	return;
}

void set_psf_vector_increment(){
	int num_inc;
	char buf[1024];
	
	num_inc = send_current_increment_vect();
	printf("Input increment for vector \n");
	printf("Current increment: %d \n", num_inc);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d ", &num_inc);
	printf("modified  increment of vector: %d \n", num_inc);
	
	if(num_inc > 0) set_current_increment_vect(num_inc);
	return;
}

void set_psf_vector_scale(){
	char buf[1024];
	float scale_input;
	scale_input = (float) send_current_scale_vect();
	
	printf("Enter scale of vector: \n");
	
	printf("Current vector scale  %.7e \n", scale_input);
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &scale_input);
	
	if ( scale_input < ZERO) scale_input = ZERO;
	printf("modified scale vector: %.7e \n", scale_input);
	
	set_current_scale_vect((double) scale_input);
	return;
}

void set_psf_vector_thickness(){
	char buf[1024];
	float thick_input;
	thick_input = (float) send_current_vector_thick();
	
	printf("Enter vector thickness: \n");
	printf("Current thick_org  %.7e \n", thick_input);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &thick_input);
	
	if ( thick_input < ZERO) thick_input = ZERO;
	printf("modified vector thickness: %.7e \n", thick_input);
	
	set_current_vector_thick((double) thick_input);
	return;
}

void set_psf_opacity(){
	float opacity;
	opacity = (float) send_current_PSF_maximum_opacity();
	
	printf("Enter PSF opacity: \n");
	opacity = set_opacity_console(opacity);
	
	set_current_PSF_constant_opacity((double) opacity);
	return;
}

void set_domain_opacity(){
	float opacity;
	opacity = (float) send_domain_surface_opacity();
	
	printf("Enter domaom opacity: \n");
	opacity = set_opacity_console(opacity);
	
	set_to_domain_surface_opacity((double) opacity);
	return;
}

void set_ele_group_opacity(){
	float opacity;
	opacity = (float) send_surf_surface_opacity();
	
	printf("Enter surafce group opacity: \n");
	opacity = set_opacity_console(opacity);
	
	set_to_ele_surface_opacity((double) opacity);
	return;
}

void set_surf_group_opacity(){
	float opacity;
	opacity = (float) send_surf_surface_opacity();
	
	printf("Enter surafce group opacity: \n");
	opacity = set_opacity_console(opacity);
	
	set_to_surf_surface_opacity((double) opacity);
	return;
}

void set_coastline_radius_console(){
	float radius;
	char buf[1024];
	float radius_org = (float) send_coastline_radius();
	
	printf("Enter coastline radius: \n");
	printf("Current radius  %.7e \n", radius_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &radius);
	printf("modified radius: %.7e \n", radius);
	
	set_to_coastline_radius((double) radius);
	return;
};

void set_domain_distance_console(){
	float distance;
	char buf[1024];
	float dist_org = (float) send_dist_domains();
	
	printf("Enter object distance: \n");
	printf("Current distance  %.7e \n", dist_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &distance);
	printf("modified distance: %.7e \n", distance);
	
	set_to_dist_domains((double) distance);
	return;
}

void set_num_color_loop_console(){
	char buf[1024];
	int num_cloop;
	int num_loop_org = send_num_of_color_loop();
	
	printf("Enter number of color: \n");
	printf("Current number  %d \n", num_loop_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d", &num_cloop);
	printf("modified number of color: %d \n", num_cloop);
	
	set_to_num_of_color_loop(num_cloop);
	return;
}

void set_node_size_console(){
	float nodesize;
	char buf[1024];
	float size_org = (float) send_node_diam();
	
	printf("Enter size of node: \n");
	printf("Current size  %.7e \n", size_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &nodesize);
	printf("modified size of node: %.7e \n", nodesize);
	
	set_to_node_diam((double) nodesize);
	return;
}

void read_psf_evolution_steps(int *ist_udt, int *ied_udt, int *inc_udt){
	char buf[LENGTHBUF];
	char *delchara;
	
	ist_udt[2] = 1;
	
	printf("Input start and end step and interval.\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	sscanf(buf, "%d %d %d", ist_udt, ied_udt, inc_udt);
	printf("start # %d, end # %d, interval # %d\n",*ist_udt, *ied_udt,*inc_udt);
};

static void read_psf_colormap_data(float *new_value, float *new_color){
	char buf[1024];
	
	printf("Input Feature point data value\n");
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", new_value);
	
	printf("Input color or opacity value (0 to 1)\n");
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", new_color);
	return;
}

void add_psf_colormap_point_data(){
	float value, color;
	read_psf_colormap_data(&value, &color);
	add_current_PSF_color_idx_list((double) value, (double) color);
	check_current_PSF_colormap_control();
	
	return;
}
void modify_psf_colormap_point_data(int i_point){
	float value, color;
	read_psf_colormap_data(&value, &color);
	set_current_PSF_color_point(i_point, (double) value, (double) color);
	check_current_PSF_colormap_control();
	return;
}

void add_psf_opacitymap_point_data(){
	float value, opacity;
	read_psf_colormap_data(&value, &opacity);
	add_current_PSF_opacity_idx_list((double) value, (double) opacity);
	check_current_PSF_colormap_control();
	return;
}
void modify_psf_opacitymap_point_data(int i_point){
	float value, opacity;
	read_psf_colormap_data(&value, &opacity);
	set_current_PSF_opacity_point(i_point, (double) value, (double) opacity);
	check_current_PSF_colormap_control();
	return;
}

vold save_colormap_file_glut(){
	char file_name[LENGTHBUF];
	char buf[LENGTHBUF];
	char *delchara;
	
	printf("Input colormap file name\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_name, buf);
	write_current_PSF_colormap_control_file(file_name);
	return;
};

void save_viewmatrix_file(){
	char file_name[LENGTHBUF];
	char buf[LENGTHBUF];
	char *delchara;
	
	printf("Input ViewMatrix file name\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_name, buf);
	write_modelview_file_glut(file_name);
	return;
};

void load_viewmatrix_file(){
	char file_name[LENGTHBUF];
	char buf[LENGTHBUF];
	char *delchara;
	
	printf("Input ViewMatrix file name\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';
	strcpy(file_name, buf);
	load_modelview_file_glut(file_name);
	return;
};


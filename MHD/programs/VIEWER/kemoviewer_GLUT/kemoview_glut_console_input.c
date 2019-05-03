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
	char *delchara;
	int id_img;
    struct kv_string *stripped_ext;
	
	printf("Input Image format\n");
	printf("   NO: No Image\n");
	printf("  PNG: PNG Images\n");
	printf("  BMP: Bitmap Images\n");
	printf("  EPS: Encapsulated PostScript\n");
	printf("   PS: PostScript\n");
	printf("  PDF:  Portable Document Format\n");
	fgets(buf,sizeof(buf),stdin);
	
	delchara=strrchr(buf,'\n');
	*delchara='\0';
    stripped_ext = kemoview_init_kvstring_by_string(buf);
	
	id_img = kemoview_set_image_file_format_id(stripped_ext);
    kemoview_free_kvstring(stripped_ext);
	return id_img;
}


void read_kemoview_data_glut(){
	char pick_command[LENGTHBUF];
	int iflag_datatype;
    struct kv_string *filename;
    struct kv_string *file_prefix;
    struct kv_string *stripped_ext;
	
    filename = kemoview_alloc_kvstring();
    kemoview_alloc_kvstringitem(LENGTHBUF, filename);
	input_file_name(filename->string);

    file_prefix = kemoview_alloc_kvstring();
    file_prefix = kemoview_alloc_kvstring();
    iflag_datatype = kemoview_set_data_format_flag(filename, file_prefix, stripped_ext);
	printf("file name: %s\n", filename->string);
	printf("file_prefix %s\n", file_prefix->string);
	printf("stripped_ext %s\n", stripped_ext->string);
    kemoview_free_kvstring(stripped_ext);
	
    if(iflag_datatype == IFLAG_FULL_MESH_GZ || iflag_datatype == IFLAG_FULL_MESH){
        kemoview_free_kvstring(filename);

        set_pickup_command(pick_command);
        kemoview_set_pick_surface_command(pick_command);
        
        filename = kemoview_alloc_kvstring();
        kemoview_alloc_kvstringitem(strlen(stripped_ext->string)+10, filename);
        strcpy(filename->string, file_prefix->string);
        strcat(filename->string, ".ksm");
        if(iflag_datatype == IFLAG_FULL_MESH_GZ){strcat(filename->string, ".gz");};
    };
	
	iflag_datatype = kemoview_open_data(filename->string);
    kemoview_free_kvstring(file_prefix);
    kemoview_free_kvstring(filename);
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
    struct kv_string *colorname;
	float range_min, range_max;
	
	int ifield = kemoview_get_PSF_field_id();
	int icomp = kemoview_get_PSF_draw_data_address();
	float data_min = (float) kemoview_get_PSF_min_data(icomp);
	float data_max = (float) kemoview_get_PSF_max_data(icomp);

    colorname = kemoview_alloc_kvstring();
	kemoview_get_PSF_field_name(colorname, ifield);
	
	input_range_from_console(colorname->string, &range_min, &range_max, data_min, data_max);
	kemoview_set_PSF_linear_colormap((double) range_min, (double) range_max);
    kemoview_free_kvstring(colorname);
	return;
}

void set_fline_range_console(){
	float range_min, range_max;
	
	int ifield = kemoview_get_fline_color_field();
	int icomp = kemoview_get_fline_color_data_adress();
	float data_min = (float) kemoview_get_fline_data_min(icomp);
	float data_max = (float) kemoview_get_fline_data_max(icomp);
    struct kv_string *colorname = kemoview_alloc_kvstring();
	kemoview_get_fline_color_data_name(colorname, ifield);
	
	input_range_from_console(colorname->string, &range_min, &range_max, data_min, data_max);	
	kemoview_set_fline_linear_colormap((double) range_min, (double) range_max);
    kemoview_free_kvstring(colorname);
	return;
}

void set_fline_thick_console(){
	float thick;
	char buf[1024];
	
	thick = (float) kemoview_get_fline_thickness();
	printf("Input fieldline thickness. \n");
	printf("Current thickness: %e \n", thick);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%e ", &thick);
	printf("modified  number of line: %e \n", thick);
	
	if(thick > 0) kemoview_set_fline_thickness((double) thick);
	return;
}

void set_num_isoline(){
	int nline;
	char buf[1024];
	
	nline = kemoview_get_PSF_num_isoline();
	printf("Input number of isolines. \n");
	printf("Current number of line: %d \n", nline);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d ", &nline);
	printf("modified  number of line: %d \n", nline);
	
	if(nline > 0) kemoview_set_PSF_num_isoline(nline);
	return;
}

void set_psf_vector_increment(){
	int num_inc;
	char buf[1024];
	
	num_inc = kemoview_get_PSF_vector_increment();
	printf("Input increment for vector \n");
	printf("Current increment: %d \n", num_inc);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d ", &num_inc);
	printf("modified  increment of vector: %d \n", num_inc);
	
	if(num_inc > 0) kemoview_set_PSF_vector_increment(num_inc);
	return;
}

void set_psf_vector_scale(){
	char buf[1024];
	float scale_input;
	scale_input = (float) kemoview_get_PSF_vector_scale();
	
	printf("Enter scale of vector: \n");
	
	printf("Current vector scale  %.7e \n", scale_input);
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &scale_input);
	
	if ( scale_input < ZERO) scale_input = ZERO;
	printf("modified scale vector: %.7e \n", scale_input);
	
	kemoview_set_PSF_vector_scale((double) scale_input);
	return;
}

void set_psf_vector_thickness(){
	char buf[1024];
	float thick_input;
	thick_input = (float) kemoview_get_PSF_vector_thickness();
	
	printf("Enter vector thickness: \n");
	printf("Current thick_org  %.7e \n", thick_input);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &thick_input);
	
	if ( thick_input < ZERO) thick_input = ZERO;
	printf("modified vector thickness: %.7e \n", thick_input);
	
	kemoview_set_PSF_vector_thickness((double) thick_input);
	return;
}

void set_psf_opacity(){
	float opacity;
	opacity = (float) kemoview_get_PSF_max_opacity();
	
	printf("Enter PSF opacity: \n");
	opacity = set_opacity_console(opacity);
	
	kemoview_set_PSF_constant_opacity((double) opacity);
	return;
}

void set_domain_opacity(){
	float opacity;
	opacity = (float) kemoview_get_domain_opacity();
	
	printf("Enter domaom opacity: \n");
	opacity = set_opacity_console(opacity);
	
	kemoview_set_domain_opacity((double) opacity);
	return;
}

void set_ele_group_opacity(){
	float opacity;
	opacity = (float) kemoview_get_surf_grp_opacity();
	
	printf("Enter surafce group opacity: \n");
	opacity = set_opacity_console(opacity);
	
	kemoview_set_ele_grp_opacity((double) opacity);
	return;
}

void set_surf_group_opacity(){
	float opacity;
	opacity = (float) kemoview_get_surf_grp_opacity();
	
	printf("Enter surafce group opacity: \n");
	opacity = set_opacity_console(opacity);
	
	kemoview_set_surf_grp_opacity((double) opacity);
	return;
}

void set_coastline_radius_console(){
	float radius;
	char buf[1024];
	float radius_org = (float) kemoview_get_coastline_radius();
	
	printf("Enter coastline radius: \n");
	printf("Current radius  %.7e \n", radius_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &radius);
	printf("modified radius: %.7e \n", radius);
	
	kemoview_set_coastline_radius((double) radius);
	return;
};

void set_background_color_console(){
    GLfloat color[4];
    float red, green, blue;
    char buf[1024];

    kemoview_get_background_color(color);
    printf("Enter Background color by (R,G,B) from 0.0 to 1.0: \n");
    printf("Corrent color:  %.7e %.7e %.7e \n", color[0], color[1], color[2]);
    fgets(buf,sizeof(buf),stdin);
    sscanf(buf,"%f %f %f", &red, &green, &blue);
    printf("New background Color (R,G,B): %.7e %.7e %.7e \n", red, green, blue);
    
    color[0] = (GLfloat) red;
    color[1] = (GLfloat) green;
    color[2] = (GLfloat) blue;

    
    draw_mesh_keep_menu();
    kemoview_set_background_color(color);
    glClear(GL_COLOR_BUFFER_BIT); 
    return;
};


void set_domain_distance_console(){
	float distance;
	char buf[1024];
	float dist_org = (float) kemoview_get_domain_distance();
	
	printf("Enter object distance: \n");
	printf("Current distance  %.7e \n", dist_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &distance);
	printf("modified distance: %.7e \n", distance);
	
	kemoview_set_domain_distance((double) distance);
	return;
}

void set_num_color_loop_console(){
	char buf[1024];
	int num_cloop;
	int num_loop_org = kemoview_get_num_of_color_loop();
	
	printf("Enter number of color: \n");
	printf("Current number  %d \n", num_loop_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%d", &num_cloop);
	printf("modified number of color: %d \n", num_cloop);
	
	kemoview_set_num_of_color_loop(num_cloop);
	return;
}

void set_node_size_console(){
	float nodesize;
	char buf[1024];
	float size_org = (float) kemoview_get_node_diamater();
	
	printf("Enter size of node: \n");
	printf("Current size  %.7e \n", size_org);
	
	fgets(buf,sizeof(buf),stdin);
	sscanf(buf,"%f", &nodesize);
	printf("modified size of node: %.7e \n", nodesize);
	
	kemoview_set_node_diamater((double) nodesize);
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

void set_psf_single_color_console(){
    double rgba[4];
	
    float red, green, blue;
    char buf[1024];
	
    printf("Enter surface color by (R,G,B) from 0.0 to 1.0: \n");
    fgets(buf,sizeof(buf),stdin);
    sscanf(buf,"%f %f %f %f", &red, &green, &blue);
	printf("New background Color (R,G,B,A): %.7e %.7e %.7e \n",
				red, green, blue);
	
	rgba[0] = (double) red;
	rgba[1] = (double) green;
	rgba[2] = (double) blue;
	rgba[3] = kemoview_get_PSF_max_opacity();
	
    kemoview_set_PSF_single_color(rgba);
	kemoview_set_PSF_patch_color_mode(SINGLE_COLOR);
	return;
}

void add_psf_colormap_point_console(){
	float value, color;
	read_psf_colormap_data(&value, &color);
	kemoview_add_PSF_color_list((double) value, (double) color);
	kemoview_check_PSF_colormap_control();
	
	return;
}
void modify_psf_colormap_point_console(int i_point){
	float value, color;
	read_psf_colormap_data(&value, &color);
	kemoview_set_PSF_color_data(i_point, (double) value, (double) color);
	kemoview_check_PSF_colormap_control();
	return;
}

void add_psf_opacitymap_point_console(){
	float value, opacity;
	read_psf_colormap_data(&value, &opacity);
	kemoview_add_PSF_opacity_list((double) value, (double) opacity);
	kemoview_check_PSF_colormap_control();
	return;
}
void modify_psf_opacitymap_point_console(int i_point){
	float value, opacity;
	read_psf_colormap_data(&value, &opacity);
	kemoview_set_PSF_opacity_data(i_point, (double) value, (double) opacity);
	kemoview_check_PSF_colormap_control();
	return;
}

void save_PSF_colormap_file_glut(){
	char buf[LENGTHBUF];
	char *delchara;
    struct kv_string *filename;
	
	printf("Input colormap file name\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';

    filename = kemoview_init_kvstring_by_string(buf);
	kemoview_write_PSF_colormap_file(filename);
    kemoview_free_kvstring(filename);
	return;
};

void load_PSF_colormap_file_glut(){
	char buf[LENGTHBUF];
	char *delchara;
    struct kv_string *filename;
	
	printf("Input colormap file name\n");
	fgets(buf,sizeof(buf),stdin);
	delchara=strrchr(buf,'\n');
	*delchara='\0';

    filename = kemoview_init_kvstring_by_string(buf);
	kemoview_read_PSF_colormap_file(filename);
    kemoview_free_kvstring(filename);
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
	kemoview_write_modelview_file(file_name);
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
	kemoview_load_modelview_file(file_name);
	return;
};


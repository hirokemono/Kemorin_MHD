
/* set_rgba_table_c.c */

#include <stdio.h>
#include <stdlib.h>
#include "set_rgba_table_c.h"


const char *label_rainbow =   "rainbow";
const char *label_bluered =   "blue_to_red";
const char *label_grayscale = "grayscale";
const char *label_sym_gray  = "symmetric_grayscale";

void set_rgb_from_value_s(struct colormap_params *cmap_s,
			double value, double *red, double *green, double *blue){
	double rnorm;
	rnorm = color_normalize_linear_segment_c(cmap_s->n_color_point, 
			cmap_s->color_data, cmap_s->color_value, value);
	
	if(cmap_s->id_color_mode == GRAYSCALE_MODE){
		color_grayscale_c(rnorm, red, green, blue);
	} else if(cmap_s->id_color_mode == SYM_GRAY_MODE){
		color_sym_grayscale_c(rnorm, red, green, blue);
	} else if(cmap_s->id_color_mode == RED_BLUE_MODE){
		color_redblue_c(rnorm, red, green, blue);
	} else {
        color_rainbow_c(rnorm, red, green, blue);
	}
	return;
}

void set_rgb_from_rgb(struct colormap_params *cmap_s,
                      double red, double green, double blue){
    
    cmap_s->single_color[0] = red;
    cmap_s->single_color[1] = green;
    cmap_s->single_color[2] = blue;
    return;
}

void set_opacity_from_value_s(struct colormap_params *cmap_s,
	double value, double *opacity){
	*opacity = color_normalize_linear_segment_c(cmap_s->n_opacity_point,
			cmap_s->opacity_data, cmap_s->opacity_value, value);
	return;
}

void set_each_color_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double color){
	cmap_s->color_data[i_point] = value;
	cmap_s->color_value[i_point] = color;
	
	return;
}

void set_each_opacity_point_s(struct colormap_params *cmap_s, 
			int i_point, double value, double opacity){
	int i;
	cmap_s->opacity_data[i_point] = value;
	cmap_s->opacity_value[i_point] = opacity;
	
	cmap_s->max_opacity = cmap_s->opacity_value[0];
	cmap_s->min_opacity = cmap_s->opacity_value[0];
	for (i=1; i<cmap_s->n_opacity_point; i++) {
		if(cmap_s->opacity_value[i] > cmap_s->max_opacity){
			cmap_s->max_opacity = cmap_s->opacity_value[i];
		}
		if(cmap_s->opacity_value[i] < cmap_s->min_opacity){
			cmap_s->min_opacity = cmap_s->opacity_value[i];
		}
		
	}
	return;
}

void set_color_mode_id_s(struct colormap_params *cmap_s, int isel){
	cmap_s->id_color_mode = isel;
	return;
}

double send_minimum_opacity_s(struct colormap_params *cmap_s){return cmap_s->min_opacity;}
double send_maximum_opacity_s(struct colormap_params *cmap_s){return cmap_s->max_opacity;}
int send_color_mode_id_s(struct colormap_params *cmap_s){return cmap_s->id_color_mode;}
int send_color_table_num_s(struct colormap_params *cmap_s){return cmap_s->n_color_point;}
int send_opacity_table_num_s(struct colormap_params *cmap_s){return cmap_s->n_opacity_point;}

void send_color_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *color){
	*value =   cmap_s->color_data[i_point];
	*color =   cmap_s->color_value[i_point];
	return;
}

void send_opacity_table_items_s(struct colormap_params *cmap_s, 
			int i_point, double *value, double *opacity){
	*value =   cmap_s->opacity_data[i_point];
	*opacity = cmap_s->opacity_value[i_point];
	return;
}

void set_linear_colormap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	realloc_color_index_list_s(cmap_s, ITWO);
	set_each_color_point_s(cmap_s, IZERO, val_min, ZERO);
	set_each_color_point_s(cmap_s, IONE,  val_max, ONE);
	return;
}
void set_constant_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max, double opaciy){
	realloc_opacity_index_list_s(cmap_s, ITWO);
	set_each_opacity_point_s(cmap_s, IZERO, val_min, opaciy);
	set_each_opacity_point_s(cmap_s, IONE,  val_max, opaciy);
	return;
}
void set_full_opacitymap(struct colormap_params *cmap_s,
			double val_min, double val_max){
	set_constant_opacitymap(cmap_s, val_min, val_max, ONE);
	return;
}


void output_colormap_control_s(FILE *fp, struct colormap_params *cmap_s){
	int i;
	double color;
	
	fprintf(fp, "  begin colormap_ctl\n");
	
	if(cmap_s->id_color_mode == RED_BLUE_MODE){
		fprintf(fp, "    colormap_mode_ctl        %s\n", label_bluered);
	} else if(cmap_s->id_color_mode == GRAYSCALE_MODE){
		fprintf(fp, "    colormap_mode_ctl        %s\n", label_grayscale);
	} else if(cmap_s->id_color_mode == SYM_GRAY_MODE){
		fprintf(fp, "    colormap_mode_ctl        %s\n", label_sym_gray);
	} else {
		fprintf(fp, "    colormap_mode_ctl        %s\n", label_rainbow);
	};
	
	fprintf(fp, "    data_mapping_ctl   colormap_list\n");
	fprintf(fp, "    array color_table_ctl    %d\n",cmap_s->n_color_point);
	for(i=0; i<cmap_s->n_color_point; i++){
		fprintf(fp, "      color_table_ctl    %.4E   %.4E\n",
				cmap_s->color_data[i], cmap_s->color_value[i]);
	}
	fprintf(fp, "    end array color_table_ctl\n");
	fprintf(fp, "!\n");
	fprintf(fp, "    opacity_style_ctl   point_linear\n");
	fprintf(fp, "    array  linear_opacity_ctl         %d\n",cmap_s->n_opacity_point);
	for(i=0; i<cmap_s->n_opacity_point; i++){
		color = color_normalize_linear_segment_c(cmap_s->n_color_point, 
					 cmap_s->color_data, cmap_s->color_value, cmap_s->opacity_data[i]);
		fprintf(fp, "      linear_opacity_ctl    %.4E   %.4E\n",
				cmap_s->opacity_data[i], cmap_s->opacity_value[i]);
	}
	fprintf(fp, "    end array linear_opacity_ctl\n");
	fprintf(fp, "    constant_opacity_ctl     %.8E\n",cmap_s->min_opacity);
	fprintf(fp, "  end colormap_ctl\n");
	fprintf(fp, "!\n");

	fprintf(fp, "  begin colorbar_ctl\n");
	fprintf(fp, "    colorbar_switch_ctl    ON\n");
	fprintf(fp, "    colorbar_scale_ctl     ON\n");
	fprintf(fp, "    iflag_zeromarker       ON\n");
	fprintf(fp, "    colorbar_range      %.4E   %.4E \n",
			cmap_s->color_data[IZERO], cmap_s->color_data[cmap_s->n_color_point-1]);
	fprintf(fp, "    font_size_ctl          1\n");
	fprintf(fp, "  end pvr_color_ctl\n");
	
	return;
}


static void input_colormap_control_s(FILE *fp, struct colormap_params *cmap_s){
    long offset;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32], ctmp2[32], ctmp3[32];            /* character buffer for reading line */
	int i, num;
	
	
    offset = skip_comment_c(fp);
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp); /* begin colormap_ctl */
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp); /* colormap_mode_ctl */
	
	if(compare_string(11, label_bluered, ctmp) > 0){
		cmap_s->id_color_mode = RED_BLUE_MODE;
	} else if(compare_string(9, label_grayscale, ctmp) > 0){
		cmap_s->id_color_mode = GRAYSCALE_MODE;
	} else if(compare_string(18, label_sym_gray, ctmp) > 0){
		cmap_s->id_color_mode = SYM_GRAY_MODE;
	} else {
		cmap_s->id_color_mode = RAINBOW_MODE;
	};
   	
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp); /* data_mapping_ctl */
	if(compare_string(13, "colormap_list", ctmp) == 0){
		printf("Something Wrong in colormap file\n)");
		return;
	}
	
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s %s", ctmp, ctmp2, ctmp3); /* array color_table_ctl */
	if(compare_string(15, "color_table_ctl", ctmp2) == 0){
		printf("Something Wrong in color_table_ctl\n)");
		return;
	}
	num = atoi(ctmp3);
	realloc_color_index_list_s(cmap_s, num);
	
	for(i=0;i<num;i++){
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%s %s %s", ctmp, ctmp2, ctmp3); /* color_table_ctl */
		cmap_s->color_data[i] =  atof(ctmp2);
		cmap_s->color_value[i] = atof(ctmp3);
	};
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s", ctmp); /* end array color_table_ctl */
    fgets(buf, LENGTHBUF, fp);        /*  !  */	
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s", ctmp, ctmp); /* opacity_style_ctl */
	if(compare_string(12, "point_linear", ctmp) == 0){
		printf("Something Wrong in opacity_style_ctl\n)");
		return;
	};
	
	
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s %s %s", ctmp, ctmp2, ctmp3); /* array linear_opacity_ctl */
	if(compare_string(18, "linear_opacity_ctl", ctmp2) == 0){
		printf("Something Wrong in linear_opacity_ctl\n)");
		return;
	}
	num = atoi(ctmp3);
	realloc_opacity_index_list_s(cmap_s, num);
	
	for(i=0;i<num;i++){
		fgets(buf, LENGTHBUF, fp);
		sscanf(buf, "%s %s %s", ctmp, ctmp2, ctmp3); /* linear_opacity_ctl */
		cmap_s->opacity_data[i] =  atof(ctmp2);
		cmap_s->opacity_value[i] = atof(ctmp3);
	};
    fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s", ctmp); /* end array linear_opacity_ctl */
    fgets(buf, LENGTHBUF, fp);
	sscanf(buf, "%s %s", ctmp, ctmp); /* constant_opacity_ctl */
	cmap_s->min_opacity = atof(ctmp);
	
	fgets(buf, LENGTHBUF, fp);
    sscanf(buf, "%s",  ctmp); /* end colormap_ctl */
	
	return;
}


void write_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	FILE *fp_cmap;
	
	printf("colormap file name: %s \n",file_name);
	if ((fp_cmap = fopen(file_name, "w")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	}
	output_colormap_control_s(fp_cmap, cmap_s);
	fclose(fp_cmap);
	
    return;
}


void read_colormap_control_file_s(const char *file_name, struct colormap_params *cmap_s){
	long  offset;
	FILE *fp_cmap;
	char buf[LENGTHBUF];      /* character buffer for reading line */
	char ctmp[32];            /* character buffer for reading line */
	
	printf("colormap file name: %s \n",file_name);
	if ((fp_cmap = fopen(file_name, "r")) == NULL) {
		fprintf(stderr, "Cannot open file!\n");
		exit (2);                    /* terminate with error message */
	}
	
    offset = skip_comment_c(fp_cmap);
    fgets(buf, LENGTHBUF, fp_cmap);
    sscanf(buf, "%s",  ctmp);
	
	input_colormap_control_s(fp_cmap, cmap_s);
	
	fclose(fp_cmap);
	return;
}

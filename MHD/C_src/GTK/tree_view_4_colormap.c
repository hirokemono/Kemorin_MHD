/*
//  tree_view_4_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_colormap.h"

void init_colormap_views_4_ctl(struct colormap_ctl_c *cmap_c, 
			struct colormap_view *color_vws){
	color_vws->colormap_mode_gtk = cmap_c->colormap_mode_ctl;
    color_vws->cmap_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    color_vws->opacity_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
	
    init_r2_clist_views(cmap_c->colortbl_list, color_vws->cmap_vws);
    init_r2_clist_views(cmap_c->linear_opacity_list, color_vws->opacity_vws);
    return;
}

void init_colormap_views_4_viewer(struct psf_menu_val *psf_current_menu, 
			struct colormap_view *color_vws){
	int icomp = psf_current_menu->icomp_draw_psf;
	color_vws->colormap_mode_gtk = (struct chara_ctl_item *) malloc(sizeof(struct chara_ctl_item));
	alloc_chara_ctl_item_c(color_vws->colormap_mode_gtk);

	color_vws->cmap_param = psf_current_menu->cmap_psf_comp[icomp];
	
	color_vws->cmap_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
	color_vws->opacity_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
	
	color_vws->cmap_vws->r2_clist_gtk =   init_real2_clist();
	color_vws->opacity_vws->r2_clist_gtk = init_real2_clist();

	sprintf(color_vws->cmap_vws->r2_clist_gtk->clist_name, "color map");
    sprintf(color_vws->cmap_vws->r2_clist_gtk->r1_name, "data");
    sprintf(color_vws->cmap_vws->r2_clist_gtk->r2_name, "color");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->clist_name, "opacity map");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->r1_name, "data");
    sprintf(color_vws->opacity_vws->r2_clist_gtk->r2_name, "opacity");
    
	int i, num;
	double value, color;
	copy_colormap_name_to_ctl(color_vws->cmap_param, 
				color_vws->colormap_mode_gtk);
	num = send_color_table_num_s(color_vws->cmap_param);
	for(i=0;i<num;i++){
		send_color_table_items_s(color_vws->cmap_param, i, &value, &color);
		append_real2_clist(value, color, color_vws->cmap_vws->r2_clist_gtk);
	};
	
	num = send_opacity_table_num_s(color_vws->cmap_param);
	for(i=0;i<num;i++){
		send_opacity_table_items_s(color_vws->cmap_param, i, &value, &color);
		append_real2_clist(value, color, color_vws->opacity_vws->r2_clist_gtk);
	};
	
	return;
};

void load_color_opacity_map_from_list(struct psf_menu_val *psf_current_menu, 
			struct colormap_view *color_vws){
	double value, color, opacity;
	int i, num;
	int icomp = psf_current_menu->icomp_draw_psf;
	num = count_real2_clist(color_vws->cmap_vws->r2_clist_gtk);
	realloc_color_index_list_s(psf_current_menu->cmap_psf_comp[icomp], num);
	for(i=0;i<num;i++){
		copy_from_real2_clist(color_vws->cmap_vws->r2_clist_gtk, 
			i, &value, &color);
		kemoview_set_PSF_color_data(i, value, color);
	}
	num = count_real2_clist(color_vws->opacity_vws->r2_clist_gtk);
	realloc_opacity_index_list_s(psf_current_menu->cmap_psf_comp[icomp], num);
	for(i=0;i<num;i++){
		copy_from_real2_clist(color_vws->opacity_vws->r2_clist_gtk, 
			i, &value, &opacity);
		set_each_PSF_opacity_point(psf_current_menu, i, value, opacity);
	}
	
	return;
}


void dealloc_colormap_views_4_viewer(struct colormap_view *color_vws){
	free(color_vws->cmap_vws);
	free(color_vws->opacity_vws);
    return;
}


void colormap_data_edited_CB(gchar *path_str, gchar *new_text,
			struct colormap_view *color_vws){
	r2_tree_value1_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
};

void colormap_color_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws){
	r2_tree_value2_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), color_vws->cmap_vws->r2_clist_gtk);
	write_real2_clist(stdout, 0, "value2 changed", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
};

void add_colormap_list_items_CB(struct colormap_view *color_vws){
	color_vws->cmap_vws->index_bc = add_r2_list_items(color_vws->cmap_vws->index_bc, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), 
				color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns added", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
};

void delete_colormap_list_items_CB(struct colormap_view *color_vws){
	delete_r2_list_items(GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns deleted", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};


void opacity_data_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws){
	r2_tree_value1_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};

void opacity_color_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws){
	r2_tree_value2_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value2 changed", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};

void add_opacity_list_items_CB(struct colormap_view *color_vws){
	color_vws->opacity_vws->index_bc = add_r2_list_items(color_vws->opacity_vws->index_bc, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns added", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};
void delete_opacity_list_items_CB(struct colormap_view *color_vws){
	delete_r2_list_items(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns deleted", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};

int set_color_mode_CB(GtkComboBox *combobox_cmap, struct colormap_view *color_vws)
{
    int index_mode = gtk_selected_combobox_index(combobox_cmap);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
    return index_mode;
}

static void draw_colormap(struct colormap_params *cmap_param, cairo_t *cr, GdkWindow *window)
{ 
    float top =     50.0;
    float left =   100.0;
    float width =   25.0;
    float height = 200.0;
    float top_s, height_s;
    
    int *i_point;
	double *d_point;
	double o_point;
    double d_bottom = 0.0;
    double d_top = 0.0;
    double d_current = 0.0;
	double max_opacity = 0.0;
    double range;
    int i;
    int num_cmap = cmap_param->n_color_point;
	int num_omap = cmap_param->n_opacity_point;
    int ntot = num_cmap + num_omap;

    i_point = (int *) calloc(ntot, sizeof(int));
	d_point = (double *) calloc(ntot, sizeof(double));
	for(i=0;i<num_cmap;i++){
        i_point[i] = i;
        d_point[i] = cmap_param->color_data[0];
	}
	
    for(i=0;i<num_omap;i++){
        i_point[i+num_cmap] = i;
		d_point[i+num_cmap] = cmap_param->opacity_data[i];
		if(cmap_param->opacity_value[i] > max_opacity){
			max_opacity = cmap_param->opacity_data[i];
		};
    }
    quicksort_double_c(d_point, i_point, 0, (ntot-1));
	
    
//    cr = gdk_cairo_create(window);
    
	/* Set colorbar */
	{        
        cairo_pattern_t *pattern1;
        cairo_pattern_t *pattern2;
        double red1, green1, blue1;
        
        /* Make colormap */
        pattern1 = cairo_pattern_create_linear((left+width), top, (left+width), (top+height));
        pattern2 = cairo_pattern_create_linear((left+2*width), top, (left+2*width), (top+height));
        top_s = top;
		height_s = 1.0;
		
		d_bottom = cmap_param->color_data[0];
		d_top = cmap_param->color_data[num_cmap-1];
		if(cmap_param->opacity_data[0] < d_bottom){
			d_bottom = cmap_param->opacity_data[0];
		};
		if(cmap_param->opacity_data[num_omap-1] < d_bottom){
			d_top = cmap_param->opacity_data[num_omap-1];
		};
		range = d_top - d_bottom;
		
        for(i=0;i<ntot-1;i++){
			set_rgb_from_value_s(cmap_param,
                                 d_point[i], &red1, &green1, &blue1);
			o_point = set_opacity_from_value_s(cmap_param, d_point[i]) / max_opacity;
			
            height_s = 1.0 - (d_point[i] - d_bottom) / range;
            cairo_pattern_add_color_stop_rgb(pattern1, height_s, red1, green1, blue1);
            cairo_pattern_add_color_stop_rgba(pattern2, height_s, red1, green1, blue1, o_point);
        }
        for(i=1;i<10;i++){
            d_current = d_bottom + (double) i * range / 10.0;
            set_rgb_from_value_s(cmap_param,
                                 d_current, &red1, &green1, &blue1);
            o_point = set_opacity_from_value_s(cmap_param, d_current) / max_opacity;
            
            height_s = 1.0 - (d_current - d_bottom) / range;
            cairo_pattern_add_color_stop_rgb(pattern1, height_s, red1, green1, blue1);
            cairo_pattern_add_color_stop_rgba(pattern2, height_s, red1, green1, blue1, o_point);
        }

        /* Set colorpattern without opacity */
            cairo_set_source(cr, pattern1);
        /* Fill rectangle */
        /* cairo_rectangle(cr, left, top, width, height); */
            cairo_rectangle(cr, left, top, width, height);
            cairo_fill_preserve(cr);
 
        /* Set colorpattern including opacity */
            cairo_set_source(cr, pattern2);
        /* Fill rectangle */
            cairo_rectangle(cr, (left+width), top, width, height);
            cairo_fill(cr);
    }
    /* Make frame */
    {
		cairo_set_line_width(cr, 2.0);
    /* Set rounded corner */
		cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
        cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
        cairo_rectangle(cr, left, top, (2.0*width), height);
        cairo_stroke(cr);
    }
	
    {
        cairo_select_font_face (cr, "Serif", CAIRO_FONT_SLANT_NORMAL,
                            CAIRO_FONT_WEIGHT_NORMAL);
        cairo_set_font_size(cr, 12);
        cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
        top_s = top;
        char *c_txt = (char *) calloc(30, sizeof(char));
        cairo_move_to(cr, left-70, top_s-20);
        cairo_show_text(cr, "Color point");
        for(i=0;i<num_cmap;i++){
			d_current = cmap_param->color_data[i];
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
            sprintf(c_txt, "%.4e", cmap_param->color_data[i]);
            cairo_move_to(cr, left-70, top_s);
            cairo_show_text(cr, c_txt);
        }
        top_s = top;
        height_s = 0.0;
        cairo_move_to(cr, left+width+30, top_s-20);
        cairo_show_text(cr, "Opacity point");
        for(i=0;i<num_omap;i++){
			d_current = cmap_param->opacity_data[i];
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
             sprintf(c_txt, "%.4e", cmap_param->opacity_data[i]);
            cairo_move_to(cr, left+width+30, top_s);
            cairo_show_text(cr, c_txt);
        }
    };

//    cairo_destroy(cr);
    free(d_point);
    free(i_point);
    return;
};

gboolean expose_event_CB(cairo_t *cr, struct colormap_view *color_vws)
{ 
	draw_colormap(color_vws->cmap_param, cr, gtk_widget_get_window(color_vws->scrolled_window));
    return FALSE;
}



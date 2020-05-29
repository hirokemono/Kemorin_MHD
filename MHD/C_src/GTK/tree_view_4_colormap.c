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

void init_colormap_views_4_viewer(struct colormap_view *color_vws){
	color_vws->cmap_param = (struct colormap_params *) kemoview_link_active_colormap_param();
	
	color_vws->colormap_mode_gtk = init_chara_ctl_item_c();
	
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
	int icomp = psf_current_menu->icomp_draw_psf;
	dup_real2_clist(color_vws->cmap_vws->r2_clist_gtk,
					psf_current_menu->cmap_psf_comp[icomp]->colormap);
	dup_real2_clist(color_vws->opacity_vws->r2_clist_gtk,
					psf_current_menu->cmap_psf_comp[icomp]->opacitymap);
	return;
}


void dealloc_colormap_views_4_viewer(struct colormap_view *color_vws){
	free(color_vws->cmap_vws);
	free(color_vws->opacity_vws);
    return;
}

static void cmap_tree_value1_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist){
	GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter, iter_prev, iter_next;
    
	double old_value1, old_value2, new_value;
	double prev_value1 = -1.0e30;
	double next_value1 =  1.0e30;
    
    if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    gtk_tree_model_get_iter(child_model, &iter_prev, child_path);  
	if(gtk_tree_model_iter_previous(child_model, &iter_prev)){
		gtk_tree_model_get(child_model, &iter_prev, COLUMN_FIELD_INDEX, &prev_value1, -1);
	};
    gtk_tree_model_get_iter(child_model, &iter_next, child_path);  
	if(gtk_tree_model_iter_next(child_model, &iter_next)){
		gtk_tree_model_get(child_model, &iter_next, COLUMN_FIELD_INDEX, &next_value1, -1);
	};
	if(new_value < prev_value1 || new_value > next_value1) new_value = old_value1;
	
	printf("Change %lf to %lf\n", old_value1, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_INDEX, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
	update_real2_clist_by_c_tbl(old_value1, old_value2, 
				new_value, old_value2, r2_clist);
};

static void cmap_tree_value2_edited(gchar *path_str, gchar *new_text,
			 GtkTreeView *r2_tree_view, struct real2_clist *r2_clist){
	GtkTreeModel *model = gtk_tree_view_get_model (r2_tree_view);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter, iter_prev, iter_next;
    
    double old_value1, old_value2, new_value;
	double prev_value2 = 0.0;
	double next_value2 = 1.0;
    
    if(sscanf(new_text, "%lf", &new_value) < 1) return;
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_INDEX, &old_value1, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_FIELD_NAME, &old_value2, -1);
    
    gtk_tree_model_get_iter(child_model, &iter_prev, child_path);
	if(gtk_tree_model_iter_previous(child_model, &iter_prev)){
		gtk_tree_model_get(child_model, &iter_prev, COLUMN_FIELD_NAME, &prev_value2, -1);
	};
    gtk_tree_model_get_iter(child_model, &iter_next, child_path);
	if(gtk_tree_model_iter_next(child_model, &iter_next)){
		gtk_tree_model_get(child_model, &iter_next, COLUMN_FIELD_NAME, &next_value2, -1);
	};
	if(new_value < prev_value2 || new_value > next_value2) new_value = old_value2;
	
    printf("Change %lf to %lf\n", old_value2, new_value);
    
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_FIELD_NAME, new_value, -1);
    gtk_tree_path_free(child_path);
    gtk_tree_path_free(path);
    
    update_real2_clist_by_c_tbl(old_value1, old_value2, old_value1, new_value, r2_clist);
}

void colormap_data_edited_CB(gchar *path_str, gchar *new_text,
			struct colormap_view *color_vws){
	cmap_tree_value1_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
};

void colormap_color_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws){
	cmap_tree_value2_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), color_vws->cmap_vws->r2_clist_gtk);
	write_real2_clist(stdout, 0, "value2 changed", color_vws->cmap_vws->r2_clist_gtk);
	
	copy_colormap_from_ctl(color_vws->colormap_mode_gtk, color_vws->cmap_vws->r2_clist_gtk,
				color_vws->cmap_param);
};

void add_colormap_list_items_CB(struct colormap_view *color_vws){
	color_vws->cmap_vws->index_bc = add_r2_list_items(GTK_TREE_VIEW(color_vws->cmap_vws->tree_view), 
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
	cmap_tree_value1_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};

void opacity_color_edited_CB(gchar *path_str, gchar *new_text, 
			struct colormap_view *color_vws){
	cmap_tree_value2_edited(path_str, new_text, 
				GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value2 changed", color_vws->opacity_vws->r2_clist_gtk);
	
	copy_opacity_from_ctl(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_param);
};

void add_opacity_list_items_CB(struct colormap_view *color_vws){
	color_vws->opacity_vws->index_bc = add_r2_list_items(GTK_TREE_VIEW(color_vws->opacity_vws->tree_view), 
									color_vws->opacity_vws->r2_clist_gtk);
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
    double range;
    int i;
    int num_cmap = count_real2_clist(cmap_param->colormap);
	int num_omap = count_real2_clist(cmap_param->opacitymap);
    int ntot = num_cmap + num_omap;
	double d1, v1;
	
	i_point = (int *) calloc(ntot, sizeof(int));
	d_point = (double *) calloc(ntot, sizeof(double));
	for(i=0;i<num_cmap;i++){
		set_from_real2_clist_at_index(i, cmap_param->colormap, &d_point[i], &v1);
        i_point[i] = i;
	}
	
	for(i=0;i<num_omap;i++){
		set_from_real2_clist_at_index(i, cmap_param->opacitymap, &d_point[i+num_cmap], &v1);
		i_point[i+num_cmap] = i;
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
		
		set_from_real2_clist_at_index(0, cmap_param->colormap, &d_bottom, &v1);
		set_from_real2_clist_at_index(num_cmap-1, cmap_param->colormap, &d_top, &v1);

		set_from_real2_clist_at_index(0, cmap_param->opacitymap, &d1, &v1);
		if(d1 < d_bottom){d_bottom = d1;};
		set_from_real2_clist_at_index(num_omap-1, cmap_param->opacitymap, &d1, &v1);
		if(d1 > d_top){d_top = d1;};
		range = d_top - d_bottom;
		
        for(i=0;i<ntot-1;i++){
			set_rgb_from_value_s(cmap_param,
                                 d_point[i], &red1, &green1, &blue1);
			o_point = set_opacity_from_value_s(cmap_param, d_point[i]) / cmap_param->max_opacity;
			
            height_s = 1.0 - (d_point[i] - d_bottom) / range;
            cairo_pattern_add_color_stop_rgb(pattern1, height_s, red1, green1, blue1);
            cairo_pattern_add_color_stop_rgba(pattern2, height_s, red1, green1, blue1, o_point);
        }
        for(i=1;i<10;i++){
            d_current = d_bottom + (double) i * range / 10.0;
            set_rgb_from_value_s(cmap_param,
                                 d_current, &red1, &green1, &blue1);
            o_point = set_opacity_from_value_s(cmap_param, d_current) / cmap_param->max_opacity;
            
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
			set_from_real2_clist_at_index(i, cmap_param->colormap, &d_current, &v1);
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
            sprintf(c_txt, "%.4e", d_current);
            cairo_move_to(cr, left-70, top_s);
            cairo_show_text(cr, c_txt);
        }
        top_s = top;
        height_s = 0.0;
        cairo_move_to(cr, left+width+30, top_s-20);
        cairo_show_text(cr, "Opacity point");
        for(i=0;i<num_omap;i++){
			set_from_real2_clist_at_index(i, cmap_param->opacitymap, &d_current, &v1);
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
             sprintf(c_txt, "%.4e", d_current);
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



/*
//  tree_view_4_pvr_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_pvr_colormap.h"

void init_colormap_views(struct colormap_ctl_c *cmap_c, struct colormap_view *color_vws){
	color_vws->index_cmap = RAINBOW_MODE;
	color_vws->colormap_mode_gtk = cmap_c->colormap_mode_ctl;
    color_vws->cmap_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(cmap_c->colortbl_list, color_vws->cmap_vws);
    color_vws->opacity_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(cmap_c->linear_opacity_list, color_vws->opacity_vws);
    return;
}

static void draw_colormap(int id_color_mode, double max_opacity, 
			struct real2_clist *colormap_clist, struct real2_clist *opacitymap_clist,
			cairo_t *cr, GdkWindow *window)
{ 
    float top =     50.0;
    float left =   100.0;
    float width =   25.0;
    float height = 200.0;
    float middle = 125.0;
    float top_s, height_s;
    
    int *i_point;
	double *d_point;
	double c_point, o_point;
    struct real2_ctl_list *head_ctl, *head_cmap;
    double d_bottom = 0.0;
    double d_current = 0.0;
    double range;
    int i;
    int num_cmap = count_chara2_clist(colormap_clist);
    int num_omap = count_chara2_clist(opacitymap_clist);
    int ntot = num_cmap + num_omap;

    i_point = (int *) calloc(ntot, sizeof(int));
	d_point = (double *) calloc(ntot, sizeof(double));
	head_cmap = &colormap_clist->r2_item_head;
	for(i=0;i<num_cmap;i++){
		head_cmap = head_cmap->_next;
        i_point[i] = i;
        d_point[i] = head_cmap->r2_item->r_data[0];
    }
	head_cmap = &opacitymap_clist->r2_item_head;
    for(i=0;i<num_omap;i++){
		head_cmap = head_cmap->_next;
        i_point[i+num_cmap] = i;
        d_point[i+num_cmap] = head_cmap->r2_item->r_data[0];
    }
    quicksort_double_c(d_point, i_point, 0, (ntot-1));
	
    
    cr = gdk_cairo_create(window);
    
	/* Set colorbar */
	{        
        cairo_pattern_t *pattern1;
        cairo_pattern_t *pattern2;
        double red1, green1, blue1;
        double red2, green2, blue2;
        
        /* Make colormap */
        pattern1 = cairo_pattern_create_linear((left+width), top, (left+width), (top+height));
        pattern2 = cairo_pattern_create_linear((left+2*width), top, (left+2*width), (top+height));
        top_s = top;
        height_s = 1.0;
        head_cmap = &colormap_clist->r2_item_head;
        d_bottom = head_cmap->_next->r2_item->r_data[0];
        range = d_point[ntot-1] - d_point[0];
        for(i=0;i<ntot-1;i++){
            d_current = d_point[i];
			set_rgb_from_value_s(id_color_mode, colormap_clist,
                                 d_current, &red1, &green1, &blue1);
			o_point = color_normalize_linear_segment_c(opacitymap_clist, d_current) / max_opacity;
			
            height_s = 1.0 - (d_current - d_bottom) / range;
            cairo_pattern_add_color_stop_rgb(pattern1, height_s, red1, green1, blue1);
            cairo_pattern_add_color_stop_rgba(pattern2, height_s, red1, green1, blue1, o_point);
        }
        for(i=1;i<10;i++){
            d_current = d_bottom + (double) i * range / 10.0;
            set_rgb_from_value_s(id_color_mode, colormap_clist,
                                 d_current, &red1, &green1, &blue1);
            o_point = color_normalize_linear_segment_c(opacitymap_clist, d_current) / max_opacity;
            
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
        char *c_txt[10];
        cairo_move_to(cr, left-70, top_s-20);
        cairo_show_text(cr, "Color point");
		head_cmap = &colormap_clist->r2_item_head;
		d_bottom = head_cmap->_next->r2_item->r_data[0];
        range = d_point[ntot-1] - d_point[0];
        for(i=0;i<num_cmap;i++){
			head_cmap = head_cmap->_next;
			d_current = head_cmap->r2_item->r_data[0];
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
            sprintf(c_txt, "%.4e", head_cmap->r2_item->r_data[0]);
            cairo_move_to(cr, left-70, top_s);
            cairo_show_text(cr, c_txt);
        }
        top_s = top;
        height_s = 0.0;
        cairo_move_to(cr, left+width+30, top_s-20);
        cairo_show_text(cr, "Opacity point");
		head_cmap = &opacitymap_clist->r2_item_head;
		d_bottom = head_cmap->_next->r2_item->r_data[0];
        for(i=0;i<num_omap;i++){
			head_cmap = head_cmap->_next;
			d_current = head_cmap->r2_item->r_data[0];
			top_s = top + height * (1.0 - (d_current - d_bottom) / range);
             sprintf(c_txt, "%.4e", head_cmap->r2_item->r_data[0]);
            cairo_move_to(cr, left+width+30, top_s);
            cairo_show_text(cr, c_txt);
        }
    };

    cairo_destroy(cr);
    free(d_point);
    free(i_point);
    return;
};

static void draw_colormap_by_ctl(cairo_t *cr, struct colormap_view *color_vws)
{ 
    color_vws->cmap_s = (struct colormap_params *)calloc(4,sizeof(struct colormap_params));
	alloc_color_index_list_s(color_vws->cmap_s, color_vws->index_cmap);
	alloc_opacity_index_list_s(color_vws->cmap_s);
	dup_real2_clist(color_vws->cmap_vws->r2_clist_gtk, color_vws->cmap_s->colormap_clist);
	dup_real2_clist(color_vws->opacity_vws->r2_clist_gtk, color_vws->cmap_s->opacitymap_clist);
	
	draw_colormap(color_vws->cmap_s->id_color_mode, color_vws->cmap_s->max_opacity, 
				color_vws->cmap_s->colormap_clist, color_vws->cmap_s->opacitymap_clist,
				cr, gtk_widget_get_window(color_vws->scrolled_window));
	
    dealloc_color_index_list_s(color_vws->cmap_s);
    dealloc_opacity_index_list_s(color_vws->cmap_s);
    free(color_vws->cmap_s);
};

static gboolean cb_expose_event(GtkWidget *widget, cairo_t *cr, gpointer user_data)
{ 
	struct colormap_view *color_vws = (struct colormap_view *) user_data;
	draw_colormap_by_ctl(cr, color_vws);
    return FALSE;
}

void colormap_data_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	r2_tree_value1_edited(path_str, new_text, 
				color_vws->cmap_vws->tree_view, color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->cmap_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void colormap_color_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	r2_tree_value2_edited(path_str, new_text, 
				color_vws->cmap_vws->tree_view, color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value2 changed", color_vws->cmap_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void add_colormap_list_items_cb(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	color_vws->cmap_vws->index_bc = add_r2_list_items(color_vws->cmap_vws->index_bc, 
				color_vws->cmap_vws->tree_view, color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns added", color_vws->cmap_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};
void delete_colormap_list_items_cb(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	delete_r2_list_items(color_vws->cmap_vws->tree_view, color_vws->cmap_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns deleted", color_vws->cmap_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void opacity_data_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	r2_tree_value1_edited(path_str, new_text, 
				color_vws->opacity_vws->tree_view, color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value1 changed", color_vws->opacity_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void opacity_color_edited_cb(GtkCellRendererText *cell, gchar *path_str,
			gchar *new_text, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	r2_tree_value2_edited(path_str, new_text, 
				color_vws->opacity_vws->tree_view, color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "value2 changed", color_vws->opacity_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};

void add_opacity_list_items_cb(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	color_vws->opacity_vws->index_bc = add_r2_list_items(color_vws->opacity_vws->index_bc, 
				color_vws->opacity_vws->tree_view, color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns added", color_vws->opacity_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};
void delete_opacity_list_items_cb(GtkButton *button, gpointer user_data){
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
	cairo_t *cr;
	
	delete_r2_list_items(color_vws->opacity_vws->tree_view, color_vws->opacity_vws->r2_clist_gtk);
    write_real2_clist(stdout, 0, "columns deleted", color_vws->opacity_vws->r2_clist_gtk);
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
};


void add_colormap_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	color_vws->cmap_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_spin_new();
	renderer_spin2 = gtk_cell_renderer_spin_new();
	
	create_real2_tree_view(color_vws->cmap_vws->tree_view, renderer_spin1, renderer_spin2);
	
    g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
                     G_CALLBACK(colormap_data_edited_cb), color_vws);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
                     G_CALLBACK(colormap_color_edited_cb), color_vws);
	
	color_vws->cmap_vws->index_bc = append_r2_list_from_ctl(color_vws->cmap_vws->index_bc,
				&color_vws->cmap_vws->r2_clist_gtk->r2_item_head, color_vws->cmap_vws->tree_view);
	
	button_add = gtk_button_new_from_stock(GTK_STOCK_ADD);
    button_delete = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	
	add_real2_list_box(color_vws->cmap_vws->tree_view, color_vws->cmap_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_colormap_list_items_cb), color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_colormap_list_items_cb), color_vws);
};

void add_opacity_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkCellRenderer *renderer_spin1;
	GtkCellRenderer *renderer_spin2;
	GtkWidget *button_add;
    GtkWidget *button_delete;
	
	color_vws->opacity_vws->tree_view = gtk_tree_view_new();
	renderer_spin1 = gtk_cell_renderer_spin_new();
	renderer_spin2 = gtk_cell_renderer_spin_new();
	
	create_real2_tree_view(color_vws->opacity_vws->tree_view, renderer_spin1, renderer_spin2);
	
    g_signal_connect(G_OBJECT(renderer_spin1), "edited", 
                     G_CALLBACK(opacity_data_edited_cb), color_vws);
    g_signal_connect(G_OBJECT(renderer_spin2), "edited", 
                     G_CALLBACK(opacity_color_edited_cb), color_vws);
	
	color_vws->opacity_vws->index_bc = append_r2_list_from_ctl(color_vws->opacity_vws->index_bc,
				&color_vws->opacity_vws->r2_clist_gtk->r2_item_head, color_vws->opacity_vws->tree_view);
	
	button_add = gtk_button_new_from_stock(GTK_STOCK_ADD);
    button_delete = gtk_button_new_from_stock(GTK_STOCK_REMOVE);
	
	add_real2_list_box(color_vws->opacity_vws->tree_view, color_vws->opacity_vws->r2_clist_gtk,
				button_add, button_delete, vbox);
	
    g_signal_connect(G_OBJECT(button_add), "clicked", 
                     G_CALLBACK(add_opacity_list_items_cb), color_vws);
    g_signal_connect(G_OBJECT(button_delete), "clicked", 
                     G_CALLBACK(delete_opacity_list_items_cb), color_vws);
};

static void set_color_mode_cb(GtkComboBox *combobox_cmap, gpointer user_data)
{
    struct colormap_view *color_vws = (struct colormap_view *) user_data;
    GtkTreeModel *model_cmap = gtk_combo_box_get_model(combobox_cmap);
    GtkTreeIter iter;
	cairo_t *cr;
    
    gchar *row_string;
    int index_field;
    int index_mode;
    
    gint idx = gtk_combo_box_get_active(combobox_cmap);
    if(idx < 0) return;
    
    GtkTreePath *path = gtk_tree_path_new_from_indices(idx, -1);
    
    gtk_tree_model_get_iter(model_cmap, &iter, path);  
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_INDEX, &index_field, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_NAME, &row_string, -1);
    gtk_tree_model_get(model_cmap, &iter, COLUMN_FIELD_MATH, &index_mode, -1);
    
	/*printf("Selected mode %d, %s\n", index_mode, row_string); */
	color_vws->index_cmap = index_mode;
	sprintf(color_vws->colormap_mode_gtk->c_tbl, "%s", row_string);
	
	draw_colormap_by_ctl(cr, color_vws);
	gtk_widget_queue_draw(color_vws->scrolled_window);
    return;
}

void add_colormp_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkWidget *expander, *Frame_1;
    GtkWidget *vbox_1, *hbox_1;
	GtkWidget *combobox_cmap;
	
	GtkWidget *label_tree;
    int index = 0;
	
	label_tree = gtk_tree_view_new();
	create_fixed_label_w_index_tree(label_tree);
    GtkTreeModel *model = gtk_tree_view_get_model (label_tree);  
    GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    index = append_ci_item_to_tree(index, &color_labels[RAINBOW_MODE][0], RAINBOW_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[GRAYSCALE_MODE][0], GRAYSCALE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[RED_BLUE_MODE][0], RED_BLUE_MODE, child_model);
	index = append_ci_item_to_tree(index, &color_labels[SYM_GRAY_MODE][0], SYM_GRAY_MODE, child_model);
	
	combobox_cmap = gtk_combo_box_new_with_model(child_model);
	child_model = gtk_cell_renderer_text_new();
	gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_cmap), child_model, TRUE);
	gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_cmap), child_model,
				"text", COLUMN_FIELD_NAME, NULL);
    g_signal_connect(G_OBJECT(combobox_cmap), "changed", 
                     G_CALLBACK(set_color_mode_cb), color_vws);
	
	
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	gtk_box_pack_start(GTK_BOX(vbox_1), combobox_cmap, FALSE, FALSE, 0);
	
	add_colormap_list_box(color_vws, vbox_1);
	add_opacity_list_box(color_vws, vbox_1);
	
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), vbox_1, TRUE, TRUE, 0);
	
	color_vws->scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(color_vws->scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(color_vws->scrolled_window, 200, 200);
    gtk_widget_set_app_paintable(color_vws->scrolled_window, TRUE);
    gtk_widget_add_events (color_vws->scrolled_window, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(color_vws->scrolled_window), "draw", 
				G_CALLBACK(cb_expose_event), color_vws);
    gtk_box_pack_start(GTK_BOX(hbox_1), color_vws->scrolled_window, TRUE, TRUE, 0);
	
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(Frame_1, hbox_1);
	
	expander = gtk_expander_new_with_mnemonic("Tako");
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
};



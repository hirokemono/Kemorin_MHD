/*
//  tree_view_4_pvr_colormap.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_pvr_colormap.h"

void init_colormap_views(struct colormap_ctl_c *cmap_c, struct colormap_view *color_vws){
    color_vws->cmap_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(cmap_c->colortbl_list, color_vws->cmap_vws);
    color_vws->opacity_vws = (struct r2_clist_view *) malloc(sizeof(struct r2_clist_view));
    init_r2_clist_views(cmap_c->linear_opacity_list, color_vws->opacity_vws);
    return;
}



static gboolean cb_expose_event(GtkWidget *widget, cairo_t *cr, gpointer user_data)
{ 
    struct colormap_view *color_vws = (struct r2_clist_view *) user_data;
    float top =     50.0;
    float left =   100.0;
    float width =   25.0;
    float height = 200.0;
    float middle = 125.0;
    float top_s, height_s;
    
    int *i_point;
    double *d_point, c_point, o_point;
    double d_min, c_min;
    double d_max, c_max;
    int i, j;
    int num_cmap = count_chara2_clist(color_vws->cmap_vws->r2_clist_gtk);
    int num_omap = count_chara2_clist(color_vws->opacity_vws->r2_clist_gtk);
    int ntot = num_cmap + num_omap;
    struct real2_ctl_list *head;

    set_from_real2_clist_at_index(0, color_vws->cmap_vws->r2_clist_gtk,
                                                 &d_min, &c_min);
    set_from_real2_clist_at_index((num_cmap-1), color_vws->cmap_vws->r2_clist_gtk,
                                  &d_max, &c_max);
	
	struct colormap_params *cmap_s;
    cmap_s = (struct colormap_params *)calloc(4,sizeof(struct colormap_params));
	alloc_color_index_list_s(cmap_s, RAINBOW_MODE, num_cmap);
	alloc_opacity_index_list_s(cmap_s, num_omap);
	
	head = &color_vws->cmap_vws->r2_clist_gtk->r2_item_head;
    for(i=0;i<cmap_s->n_color_point;i++){
        head = head->_next;
        cmap_s->color_data[i] = head->r2_item->r_data[0];
        cmap_s->color_value[i] = head->r2_item->r_data[1];
    }
    head = &color_vws->opacity_vws->r2_clist_gtk->r2_item_head;
    cmap_s->max_opacity = 0.0;
    for(i=0;i<cmap_s->n_opacity_point;i++){
        head = head->_next;
        cmap_s->opacity_data[i] = head->r2_item->r_data[0];
        cmap_s->opacity_value[i] = head->r2_item->r_data[1];
		if(cmap_s->opacity_value[i] > cmap_s->max_opacity) cmap_s->max_opacity = cmap_s->opacity_value[i];
    }
	
	/* copy_colormap_from_ctl(cmap_c, cmap_s); */
    i_point = (int *) calloc(ntot, sizeof(int));
    d_point = (double *) calloc(ntot, sizeof(double));
    for(i=0;i<cmap_s->n_color_point;i++){
        i_point[i] = i;
        d_point[i] = cmap_s->color_data[i];
    }
    for(i=0;i<cmap_s->n_opacity_point;i++){
        i_point[i+num_cmap] = i;
        d_point[i+num_cmap] = cmap_s->opacity_data[i];
    }
	
    quicksort_double_c(d_point, i_point, 0, (ntot-1));
    
    cr = gdk_cairo_create(gtk_widget_get_window(widget));
    
    //グラデーション付き長方形
    {        
        cairo_pattern_t *pattern1;
        cairo_pattern_t *pattern2;
        double red1, green1, blue1;
        double red2, green2, blue2;
        
        //グラデーションを作る
        pattern1 = cairo_pattern_create_linear((left+width), top, (left+width), (top+height));
        pattern2 = cairo_pattern_create_linear((left+2*width), top, (left+2*width), (top+height));
        top_s = top;
        height_s = 0.0;
        for(i=0;i<ntot-1;i++){
			set_rgb_from_value_s(cmap_s, d_point[i], &red1, &green1, &blue1);
			o_point = set_opacity_from_value_s(cmap_s, d_point[i]) / cmap_s->max_opacity;
			
			top_s = top_s + height_s;
            height_s = height * (d_point[i+1] - d_point[i]) / (d_point[ntot-1] - d_point[0]);
            cairo_pattern_add_color_stop_rgb(pattern1, (top_s-top)/height, red1, green1, blue1);
            cairo_pattern_add_color_stop_rgba(pattern2, (top_s-top)/height, red1, green1, blue1, o_point);
        }

        //上記のグラデーションを設定する
            cairo_set_source(cr, pattern1);
        //上記のグラデーションで長方形を塗りつぶす
        /* cairo_rectangle(cr, left, top, width, height); */
            cairo_rectangle(cr, left, top, width, height);
            cairo_fill_preserve(cr);
 
        //上記のグラデーションを設定する
            cairo_set_source(cr, pattern2);
        //上記のグラデーションで長方形を塗りつぶす
            cairo_rectangle(cr, (left+width), top, width, height);
        //上記のグラデーションを設定する
            cairo_set_source(cr, pattern2);
            cairo_fill(cr);
    }
    //外枠（青）を作る
    {
    //線の太さ
		cairo_set_line_width(cr, 2.0);
    //角を丸くする
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
        height_s = 0.0;
        char *c_txt[10];
        cairo_move_to(cr, left-70, top_s-20);
        cairo_show_text(cr, "Color point");
        for(i=0;i<cmap_s->n_color_point;i++){
            top_s = top_s + height_s;
			height_s = height * (cmap_s->color_data[i+1] - cmap_s->color_data[i])
					/ (d_point[ntot-1] - d_point[0]);
            sprintf(c_txt, "%.4e", cmap_s->color_data[i]);
            cairo_move_to(cr, left-70, top_s);
            cairo_show_text(cr, c_txt);
        }
        top_s = top;
        height_s = 0.0;
        cairo_move_to(cr, left+width+30, top_s-20);
        cairo_show_text(cr, "Opacity point");
        for(i=0;i<cmap_s->n_opacity_point;i++){
            top_s = top_s + height_s;
			height_s = height * (cmap_s->opacity_data[i+1] - cmap_s->opacity_data[i]) 
					/ (d_point[ntot-1] - d_point[0]);
            sprintf(c_txt, "%.4e", cmap_s->opacity_data[i]);
            cairo_move_to(cr, left+width+30, top_s);
            cairo_show_text(cr, c_txt);
        }
    };

    cairo_destroy(cr);
    return FALSE;
};

void add_colormp_list_box(struct colormap_view *color_vws, GtkWidget *vbox){
	GtkWidget *expander, *Frame_1;
    GtkWidget *vbox_1, *hbox_1;
    GtkWidget *scrolled_window;
	
    vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	add_real2_list_box_w_addbottun(color_vws->cmap_vws, vbox_1);
	add_real2_list_box_w_addbottun(color_vws->opacity_vws, vbox_1);
	
	hbox_1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(hbox_1), vbox_1, TRUE, TRUE, 0);
	
	scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_widget_set_size_request(scrolled_window, 200, 200);
    gtk_widget_set_app_paintable(scrolled_window, TRUE);
    gtk_widget_add_events (scrolled_window, GDK_BUTTON_PRESS_MASK);
	g_signal_connect(G_OBJECT(scrolled_window), "draw", 
				G_CALLBACK(cb_expose_event), color_vws);
    gtk_box_pack_start(GTK_BOX(hbox_1), scrolled_window, TRUE, TRUE, 0);
	
    
	Frame_1 = gtk_frame_new("");
	gtk_frame_set_shadow_type(GTK_FRAME(Frame_1), GTK_SHADOW_IN);
	gtk_container_add(GTK_BOX(Frame_1), hbox_1);
	
	expander = gtk_expander_new_with_mnemonic("Tako");
	gtk_container_add(GTK_CONTAINER(expander), Frame_1);
	gtk_box_pack_start(GTK_BOX(vbox), expander, TRUE, TRUE, 0);
};
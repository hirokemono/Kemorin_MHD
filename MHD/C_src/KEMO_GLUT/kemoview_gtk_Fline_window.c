/*
 *  kemoview_gtk_Fline_window.c
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_gtk_Fline_window.h"


static void close_fline_CB(GtkButton *button, gpointer user_data){
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(button), "flinemenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(button), "kemoview_gl");

	kemoview_close_fieldline_view(kemo_gl->kemoview_data);
	
    gtk_widget_destroy(fline_gmenu->flineWin);
    activate_evolution_menu(kemo_gl->kemoview_data, itemTEvo);
    draw_full_gl(kemo_gl);
};

static void fline_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_field);
    
    kemoview_set_VIZ_field_param(index_mode,
                                 fline_gmenu->iflag_flinemode,
                                 FIELD_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    kemoview_set_VIZ_field_param(index_mode,
                                 fline_gmenu->iflag_flinemode,
                                 COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    
    replace_fline_frame(kemo_gl, fline_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
    return;
};

static void fline_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *fline_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "fline_view");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_comp);
    kemoview_set_VIZ_field_param(index_mode,
                                 fline_gmenu->iflag_flinemode,
                                 COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    
    replace_fline_frame(kemo_gl, fline_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
    return;
};


static void tracer_field_select_CB(GtkComboBox *combobox_field, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *tracer_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "tracerMenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_field);
    
    kemoview_set_VIZ_field_param(index_mode,
                                 tracer_gmenu->iflag_flinemode,
                                 FIELD_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    kemoview_set_VIZ_field_param(index_mode,
                                 tracer_gmenu->iflag_flinemode,
                                 COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    
    replace_tracer_frame(kemo_gl, tracer_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
    return;
};

static void tracer_component_select_CB(GtkComboBox *combobox_comp, gpointer user_data)
{
    GtkWidget *itemTEvo = GTK_WIDGET(user_data);
    struct fieldline_gtk_menu *tracer_gmenu
            = (struct fieldline_gtk_menu *) g_object_get_data(G_OBJECT(user_data), "tracerMenu");
    struct kemoviewer_gl_type *kemo_gl
            = (struct kemoviewer_gl_type *) g_object_get_data(G_OBJECT(user_data), "kemoview_gl");
    int index_mode = gtk_selected_combobox_index(combobox_comp);
    kemoview_set_VIZ_field_param(index_mode,
                                 tracer_gmenu->iflag_flinemode,
                                 COMPONENT_SEL_FLAG,
                                 kemo_gl->kemoview_data);
    
    replace_tracer_frame(kemo_gl, tracer_gmenu, itemTEvo);
    draw_full_gl(kemo_gl);
    return;
};


GtkWidget * fline_draw_field_box(struct kemoviewer_gl_type *kemo_gl,
                                 struct fieldline_gtk_menu *fline_gmenu){
    struct kv_string *colorname = kemoview_alloc_kvstring();
    int num_field = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                 fline_gmenu->iflag_flinemode,
                                                 NUM_FIELD_FLAG);
    int if_fline =  kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                 fline_gmenu->iflag_flinemode,
                                                 FIELD_SEL_FLAG);
    fline_gmenu->label_tree_field = create_fixed_label_w_index_tree();
    GtkTreeModel *model_field = gtk_tree_view_get_model(GTK_TREE_VIEW(fline_gmenu->label_tree_field));
    GtkTreeModel *child_model_field = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_field));
    int index = 0;
    for(int ifld=0;ifld<num_field;ifld++){
        kemoview_get_VIZ_field_name(kemo_gl->kemoview_data,
                                    fline_gmenu->iflag_flinemode,
                                    colorname, ifld);
        index = append_ci_item_to_tree(index, colorname->string, ifld, child_model_field);
    };
    
    fline_gmenu->renderer_field = gtk_cell_renderer_text_new();
    GtkWidget *combobox_field = gtk_combo_box_new();
    gtk_combo_box_set_model(GTK_COMBO_BOX(combobox_field), child_model_field);
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_field), if_fline);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_field), 
                               fline_gmenu->renderer_field, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_field), 
                                   fline_gmenu->renderer_field,
                                   "text", COLUMN_FIELD_NAME, NULL);
    return combobox_field;
}

GtkWidget * fline_draw_component_combobox(struct kemoviewer_gl_type *kemo_gl,
                                          struct fieldline_gtk_menu *fline_gmenu){
    char comp_name[128];
    
    int if_fline = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                fline_gmenu->iflag_flinemode,
                                                FIELD_SEL_FLAG);
    int ic_fline = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                fline_gmenu->iflag_flinemode,
                                                COMPONENT_SEL_FLAG);
    int ncomp =  kemoview_get_VIZ_num_component(kemo_gl->kemoview_data,
                                                fline_gmenu->iflag_flinemode, if_fline);
    
    fline_gmenu->label_tree_comp = create_fixed_label_w_index_tree();
    GtkTreeModel *model_comp = gtk_tree_view_get_model(GTK_TREE_VIEW(fline_gmenu->label_tree_comp));
    GtkTreeModel *child_model_comp = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model_comp));
    int id_coord = kemoview_get_VIZ_field_param(kemo_gl->kemoview_data,
                                                fline_gmenu->iflag_flinemode,
                                                COORDINATE_FLAG);
    int index = 0;
    for(int icomp=0;icomp<ncomp;icomp++){
        set_PSF_component_name(ncomp, id_coord, icomp, comp_name);
        index = append_ci_item_to_tree(index, comp_name, icomp, child_model_comp);
    };
    
    GtkWidget *combobox_comp = gtk_combo_box_new_with_model(child_model_comp);
    fline_gmenu->renderer_comp = gtk_cell_renderer_text_new();
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox_comp), ic_fline);
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox_comp),
                               fline_gmenu->renderer_comp, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox_comp),
                                   fline_gmenu->renderer_comp,
                                   "text", COLUMN_FIELD_NAME, NULL);
    return combobox_comp;
}


static void set_fieldline_menu_box(struct kemoviewer_gl_type *kemo_gl,
                                   struct fieldline_gtk_menu *fline_gmenu,
                                   GtkWidget *itemTEvo){
    fline_gmenu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(fline_gmenu->closeButton), "flinemenu", (gpointer) fline_gmenu);
    g_signal_connect(G_OBJECT(fline_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), itemTEvo);
	    
	init_fieldline_menu_hbox(kemo_gl, fline_gmenu);
    
    g_object_set_data(G_OBJECT(itemTEvo), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(itemTEvo), "flinemenu", (gpointer) fline_gmenu);
    fline_gmenu->combobox_field = fline_draw_field_box(kemo_gl, fline_gmenu);
    fline_gmenu->combobox_comp = fline_draw_component_combobox(kemo_gl, fline_gmenu);
    g_signal_connect(G_OBJECT(fline_gmenu->combobox_field), "changed",
                G_CALLBACK(fline_field_select_CB), (gpointer) itemTEvo);
    g_signal_connect(G_OBJECT(fline_gmenu->combobox_comp), "changed",
                G_CALLBACK(fline_component_select_CB), (gpointer) itemTEvo);
    
    int itype_fline = kemoview_get_line_type_flag(kemo_gl->kemoview_data);
    if(itype_fline == 0){
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_tube), FALSE);
    } else {
        gtk_switch_set_active(GTK_SWITCH(fline_gmenu->switch_tube), TRUE);
    };
    
    set_gtk_fieldline_menu(kemo_gl, fline_gmenu);
	return;
}

static void set_tracer_menu_box(struct kemoviewer_gl_type *kemo_gl,
                                struct fieldline_gtk_menu *tracer_gmenu,
                                GtkWidget *itemTEvo){
    tracer_gmenu->closeButton = gtk_button_new_with_label("Close Fieldline");

    g_object_set_data(G_OBJECT(tracer_gmenu->closeButton), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(tracer_gmenu->closeButton), "flinemenu", (gpointer) tracer_gmenu);
    g_signal_connect(G_OBJECT(tracer_gmenu->closeButton), "clicked",
                     G_CALLBACK(close_fline_CB), itemTEvo);
        
    init_tracer_menu_hbox(kemo_gl, tracer_gmenu);
    
    g_object_set_data(G_OBJECT(itemTEvo), "kemoview_gl", (gpointer) kemo_gl);
    g_object_set_data(G_OBJECT(itemTEvo), "tracerMenu", (gpointer) tracer_gmenu);
    tracer_gmenu->combobox_field = fline_draw_field_box(kemo_gl, tracer_gmenu);
    tracer_gmenu->combobox_comp = fline_draw_component_combobox(kemo_gl, tracer_gmenu);
    g_signal_connect(G_OBJECT(tracer_gmenu->combobox_field), "changed",
                G_CALLBACK(tracer_field_select_CB), (gpointer) itemTEvo);
    g_signal_connect(G_OBJECT(tracer_gmenu->combobox_comp), "changed",
                G_CALLBACK(tracer_component_select_CB), (gpointer) itemTEvo);
    
    set_gtk_fieldline_menu(kemo_gl, tracer_gmenu);
    return;
}

GtkWidget *  init_fline_frame(struct kemoviewer_gl_type *kemo_gl,
                              struct fieldline_gtk_menu *fline_gmenu,
                              GtkWidget *itemTEvo){
    GtkWidget * flineFrame;
    fline_gmenu->fline_color_vws = alloc_colormap_view();
    set_fieldline_menu_box(kemo_gl, fline_gmenu, itemTEvo);
    flineFrame = pack_fieldline_menu_frame(fline_gmenu);
    return flineFrame;
}

void replace_fline_frame(struct kemoviewer_gl_type *kemo_gl,
                        struct fieldline_gtk_menu *fline_gmenu,
                        GtkWidget *itemTEvo){
    gtk_widget_destroy(fline_gmenu->fline_frame);
    fline_gmenu->fline_frame = init_fline_frame(kemo_gl, fline_gmenu, itemTEvo);
    gtk_container_add(GTK_CONTAINER(fline_gmenu->flineWin), fline_gmenu->fline_frame);
    gtk_widget_show_all(fline_gmenu->flineWin);
    gtk_widget_queue_draw(fline_gmenu->flineWin);
    return;
}

void init_fline_window(struct kemoviewer_gl_type *kemo_gl,
                       struct fieldline_gtk_menu *fline_gmenu,
                       GtkWidget *main_window, GtkWidget *itemTEvo){
    fline_gmenu->iflag_flineBox = kemoview_get_VIZ_draw_flags(kemo_gl->kemoview_data,
                                                             FIELDLINE_RENDERING);
    if(fline_gmenu->iflag_flineBox == 0){return;};

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    fline_gmenu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(fline_gmenu->flineWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(fline_gmenu->flineWin), "Fieldline");
    gtk_widget_set_size_request(fline_gmenu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(fline_gmenu->flineWin), 5);

    fline_gmenu->fline_frame = init_fline_frame(kemo_gl, fline_gmenu, itemTEvo);
    
    gtk_container_add(GTK_CONTAINER(fline_gmenu->flineWin), fline_gmenu->fline_frame);
    gtk_widget_show_all(fline_gmenu->flineWin);
    return;
}

GtkWidget * init_tracer_frame(struct kemoviewer_gl_type *kemo_gl,
                              struct fieldline_gtk_menu *tracer_gmenu,
                              GtkWidget *itemTEvo){
    GtkWidget * tracerFrame;
    tracer_gmenu->fline_color_vws = alloc_colormap_view();
    set_tracer_menu_box(kemo_gl, tracer_gmenu, itemTEvo);
    tracerFrame = pack_tracer_menu_frame(tracer_gmenu);
    return tracerFrame;
}

void replace_tracer_frame(struct kemoviewer_gl_type *kemo_gl,
                          struct fieldline_gtk_menu *tracer_gmenu,
                          GtkWidget *itemTEvo){
    gtk_widget_destroy(tracer_gmenu->fline_frame);
    tracer_gmenu->fline_frame = init_tracer_frame(kemo_gl, tracer_gmenu, itemTEvo);
    gtk_container_add(GTK_CONTAINER(tracer_gmenu->flineWin), tracer_gmenu->fline_frame);
    gtk_widget_show_all(tracer_gmenu->flineWin);
    gtk_widget_queue_draw(tracer_gmenu->flineWin);
    return;
}


void init_tracer_window(struct kemoviewer_gl_type *kemo_gl,
                        struct fieldline_gtk_menu *tracer_gmenu,
                        GtkWidget *main_window, GtkWidget *itemTEvo){
    tracer_gmenu->iflag_flineBox = kemoview_get_VIZ_draw_flags(kemo_gl->kemoview_data,
                                                               TRACER_RENDERING);
    if(tracer_gmenu->iflag_flineBox == 0){return;};

    gint win_upperleft[2], size_xy[2];
    gtk_window_get_position(GTK_WINDOW(main_window), &win_upperleft[0], &win_upperleft[1]);
    gtk_window_get_size(GTK_WINDOW(main_window), &size_xy[0], &size_xy[1]);

    tracer_gmenu->flineWin = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_move(GTK_WINDOW(tracer_gmenu->flineWin),
                    win_upperleft[0], (win_upperleft[1]+size_xy[1]+56));
    gtk_window_set_title(GTK_WINDOW(tracer_gmenu->flineWin), "Tracer");
    gtk_widget_set_size_request(tracer_gmenu->flineWin, 150, -1);
    gtk_container_set_border_width(GTK_CONTAINER(tracer_gmenu->flineWin), 5);
    
    tracer_gmenu->fline_frame = init_tracer_frame(kemo_gl, tracer_gmenu, itemTEvo);
    
    gtk_container_add(GTK_CONTAINER(tracer_gmenu->flineWin), tracer_gmenu->fline_frame);
    gtk_widget_show_all(tracer_gmenu->flineWin);
    return;
}

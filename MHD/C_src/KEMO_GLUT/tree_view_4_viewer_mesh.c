/*
//  tree_view_4_viewer_mesh.c
//  
//
//  Created by Hiroaki Matsui on 2018/08/19.
*/

#include "tree_view_4_viewer_mesh.h"

void init_mesh_views_4_viewer(struct kemoview_mesh_view *mesh_vws){
    int i;
	char tmp_name[128];
	struct kv_string *groupname;
	
    mesh_vws->domain_vws =  (struct ci3_clist_view *)  malloc(sizeof(struct ci3_clist_view));
    mesh_vws->nod_grp_vws =  (struct ci_clist_view *)  malloc(sizeof(struct ci_clist_view));
    mesh_vws->ele_grp_vws =  (struct ci3_clist_view *) malloc(sizeof(struct ci3_clist_view));
    mesh_vws->surf_grp_vws = (struct ci3_clist_view *) malloc(sizeof(struct ci3_clist_view));
	
    mesh_vws->domain_vws->ci3_clist_gtk =  (struct chara_int3_clist *)  malloc(sizeof(struct chara_int3_clist));
    mesh_vws->nod_grp_vws->ci_clist_gtk =  (struct chara_int_clist *)  malloc(sizeof(struct chara_int_clist));
    mesh_vws->ele_grp_vws->ci3_clist_gtk =  (struct chara_int3_clist *) malloc(sizeof(struct chara_int3_clist));
    mesh_vws->surf_grp_vws->ci3_clist_gtk = (struct chara_int3_clist *) malloc(sizeof(struct chara_int3_clist));
	
	init_chara_int3_clist(mesh_vws->domain_vws->ci3_clist_gtk);
	init_chara_int_clist(mesh_vws->nod_grp_vws->ci_clist_gtk);
	init_chara_int3_clist(mesh_vws->ele_grp_vws->ci3_clist_gtk);
	init_chara_int3_clist(mesh_vws->surf_grp_vws->ci3_clist_gtk);
	
	sprintf(mesh_vws->domain_vws->ci3_clist_gtk->clist_name,"%s", "Domain Data");
	sprintf(mesh_vws->domain_vws->ci3_clist_gtk->c1_name,"%s", "Name");
	sprintf(mesh_vws->domain_vws->ci3_clist_gtk->i1_name,"%s", "Patch");
	sprintf(mesh_vws->domain_vws->ci3_clist_gtk->i2_name,"%s", "Grid");
	sprintf(mesh_vws->domain_vws->ci3_clist_gtk->i3_name,"%s", "Node");
	
	sprintf(mesh_vws->nod_grp_vws->ci_clist_gtk->clist_name,"%s", "Node group");
	sprintf(mesh_vws->nod_grp_vws->ci_clist_gtk->c1_name,"%s", "Name");
	sprintf(mesh_vws->nod_grp_vws->ci_clist_gtk->i1_name,"%s", "Node");
	
	sprintf(mesh_vws->ele_grp_vws->ci3_clist_gtk->clist_name,"%s", "Element group");
	sprintf(mesh_vws->ele_grp_vws->ci3_clist_gtk->c1_name,"%s", "Name");
	sprintf(mesh_vws->ele_grp_vws->ci3_clist_gtk->i1_name,"%s", "Patch");
	sprintf(mesh_vws->ele_grp_vws->ci3_clist_gtk->i2_name,"%s", "Grid");
	sprintf(mesh_vws->ele_grp_vws->ci3_clist_gtk->i3_name,"%s", "Node");
	
	sprintf(mesh_vws->surf_grp_vws->ci3_clist_gtk->clist_name,"%s", "Surface group");
	sprintf(mesh_vws->surf_grp_vws->ci3_clist_gtk->c1_name,"%s", "Name");
	sprintf(mesh_vws->surf_grp_vws->ci3_clist_gtk->i1_name,"%s", "Patch");
	sprintf(mesh_vws->surf_grp_vws->ci3_clist_gtk->i2_name,"%s", "Grid");
	sprintf(mesh_vws->surf_grp_vws->ci3_clist_gtk->i3_name,"%s", "Node");
	
	for(i=0;i<kemoview_get_num_of_mesh_group(DOMAIN_FLAG);i++){
		sprintf(tmp_name, "Domain %d", i);
		append_chara_int3_clist(tmp_name, 
					kemoview_get_draw_mesh_item(DOMAIN_FLAG, SURFSOLID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(DOMAIN_FLAG, SURFGRID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(DOMAIN_FLAG, SURFNOD_TOGGLE, i), 
					mesh_vws->domain_vws->ci3_clist_gtk);
	};
	for(i=0;i<kemoview_get_num_of_mesh_group(NODE_GRP_FLAG);i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_node_grp_name(groupname, i);
		append_chara_int_clist(groupname, 
					kemoview_get_draw_mesh_item(NODE_GRP_FLAG, SURFSOLID_TOGGLE, i),
					mesh_vws->nod_grp_vws->ci_clist_gtk);
        kemoview_free_kvstring(groupname);
	};
	for(i=0;i<kemoview_get_num_of_mesh_group(ELEM_GRP_FLAG);i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_ele_grp_name(groupname, i);
		append_chara_int3_clist(groupname->string, 
					kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i), 
					mesh_vws->ele_grp_vws->ci3_clist_gtk);
        kemoview_free_kvstring(groupname);
	};
	for(i=0;i<kemoview_get_num_of_mesh_group(SURF_GRP_FLAG);i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_surf_grp_name(groupname, i);
		append_chara_int3_clist(groupname->string, 
					kemoview_get_draw_mesh_item(SURF_GRP_FLAG, SURFSOLID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(SURF_GRP_FLAG, SURFGRID_TOGGLE, i), 
					kemoview_get_draw_mesh_item(SURF_GRP_FLAG, SURFNOD_TOGGLE, i), 
					mesh_vws->surf_grp_vws->ci3_clist_gtk);
        kemoview_free_kvstring(groupname);
	};
	return;
}


void dealloc_mesh_views_4_viewer(struct kemoview_mesh_view *mesh_vws){
	clear_chara_int3_clist(mesh_vws->surf_grp_vws->ci3_clist_gtk);
	clear_chara_int3_clist(mesh_vws->ele_grp_vws->ci3_clist_gtk);
	clear_chara_int_clist(mesh_vws->nod_grp_vws->ci_clist_gtk);
	clear_chara_int3_clist(mesh_vws->domain_vws->ci3_clist_gtk);
	
	free(mesh_vws->surf_grp_vws->ci3_clist_gtk);
	free(mesh_vws->ele_grp_vws->ci3_clist_gtk);
	free(mesh_vws->nod_grp_vws->ci_clist_gtk);
	free(mesh_vws->domain_vws->ci3_clist_gtk);
	
	free(mesh_vws->surf_grp_vws);
	free(mesh_vws->ele_grp_vws);
	free(mesh_vws->nod_grp_vws);
	free(mesh_vws->domain_vws);
    return;
}

/* Append new data at the end of list */
void append_grp_model_data(int index, struct ci3_clist_view *grp_vws, 
			GtkListStore *child_model)
{
    GtkTreeIter iter;
	int i1_out, i2_out, i3_out;
	char tmp_name[128];
	
	set_from_chara_int3_clist_at_index(index, grp_vws->ci3_clist_gtk, 
				tmp_name, &i1_out, &i2_out, &i3_out);
    gtk_list_store_append(child_model, &iter);
    gtk_list_store_set(child_model, &iter,
                       COLUMN_MESH_INDEX, index,
                       COLUMN_MESH_NAME,  tmp_name,
                       COLUMN_MESH_THIRD, (gboolean) i1_out,
                       COLUMN_MESH_FORTH, (gboolean) i2_out,
                       COLUMN_MESH_FIFTH, (gboolean) i3_out,
				-1);
	return;
}

void append_node_grp_model_data(int index, struct ci_clist_view *nod_grp_vws,
			GtkListStore *child_model)
{
    GtkTreeIter iter;
	int i1_out;
	char tmp_name[32];
	
	set_from_chara_int_clist_at_index(index, nod_grp_vws->ci_clist_gtk, 
				tmp_name, &i1_out);
    gtk_list_store_append(child_model, &iter);
    gtk_list_store_set(child_model, &iter,
                       COLUMN_MESH_INDEX, index,
                       COLUMN_MESH_NAME, tmp_name,
                       COLUMN_MESH_THIRD, (gboolean) i1_out,
				-1);
	return;
}

int toggle_draw_patch_switch(gchar *path_str, gpointer user_data, 
			int *index1_for_toggle){
	struct ci3_clist_view *grp_vws = (struct ci3_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
	int index;
	int index2_for_toggle, index3_for_toggle;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_INDEX, &index, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_NAME,  &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_THIRD,  index1_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FORTH, &index2_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FIFTH, &index3_for_toggle, -1);
    
    printf("toggle_draw_patch %d, %s: \n", index, row_string);
    
    *index1_for_toggle = (*index1_for_toggle + 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MESH_THIRD, (gboolean) *index1_for_toggle, -1);
    gtk_tree_path_free(child_path);
	gtk_tree_path_free(path);
	
	update_chara_int3_clist_by_index(index, row_string, 
				*index1_for_toggle, index2_for_toggle, index3_for_toggle, 
				grp_vws->ci3_clist_gtk);
	return index;
}

int toggle_draw_grid_switch(gchar *path_str, gpointer user_data, 
			int *index2_for_toggle){
	struct ci3_clist_view *grp_vws = (struct ci3_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
	int index;
	int index1_for_toggle, index3_for_toggle;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_INDEX,  &index, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_NAME,   &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_THIRD,  &index1_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FORTH,  index2_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FIFTH,  &index3_for_toggle, -1);
    
    printf("toggle_draw_grid %d, %s: \n", index, row_string);
    
    *index2_for_toggle = (*index2_for_toggle + 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MESH_FORTH, (gboolean) *index2_for_toggle, -1);
    gtk_tree_path_free(child_path);  
	gtk_tree_path_free(path);  
	
	update_chara_int3_clist_by_index(index, row_string, 
				index1_for_toggle, *index2_for_toggle, index3_for_toggle, 
				grp_vws->ci3_clist_gtk);
	return index;
}

int toggle_draw_node_switch(gchar *path_str, gpointer user_data, 
			int *index3_for_toggle){
	struct ci3_clist_view *grp_vws = (struct ci3_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
	int index;
	int index1_for_toggle, index2_for_toggle;
    
    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_INDEX,  &index, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_NAME,   &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_THIRD,  &index1_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FORTH,  &index2_for_toggle, -1);
	gtk_tree_model_get(child_model, &iter, COLUMN_MESH_FIFTH,  index3_for_toggle, -1);
    
    printf("toggle_draw_node %d, %s: \n", index, row_string);
	
    *index3_for_toggle = (*index3_for_toggle + 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MESH_FIFTH, (gboolean) *index3_for_toggle, -1);
    gtk_tree_path_free(child_path);  
	gtk_tree_path_free(path);  
	
	update_chara_int3_clist_by_index(index, row_string, 
				index1_for_toggle, index2_for_toggle, *index3_for_toggle, 
				grp_vws->ci3_clist_gtk);
	return index;
}

int toggle_draw_nod_grp_node_switch(gchar *path_str, gpointer user_data,
			int *index1_for_toggle){
	struct ci_clist_view *nod_grp_vws = (struct ci_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(nod_grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreePath *path = gtk_tree_path_new_from_string (path_str);  
    GtkTreePath *child_path = gtk_tree_model_sort_convert_path_to_child_path(GTK_TREE_MODEL_SORT(model), path);
    GtkTreeIter iter;

    gchar *row_string;
	int index_grp;

    gtk_tree_model_get_iter(child_model, &iter, child_path);  
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_INDEX,  &index_grp, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_NAME,   &row_string, -1);
    gtk_tree_model_get(child_model, &iter, COLUMN_MESH_THIRD,   index1_for_toggle, -1);
    
    printf("toggle_draw_node %d, %s: \n", index_grp, row_string);
    
    *index1_for_toggle = (*index1_for_toggle + 1) % 2;
    gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
                       COLUMN_MESH_THIRD, (gboolean) *index1_for_toggle, -1);
    gtk_tree_path_free(child_path);  
    gtk_tree_path_free(path);  

	update_chara_int_clist_by_index(index_grp, row_string, 
			*index1_for_toggle, nod_grp_vws->ci_clist_gtk);
	return index_grp;
}


int set_all_draw_flags(int iflag, int iflag_column, gpointer user_data)
{
	struct ci3_clist_view *grp_vws = (struct ci3_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreeIter iter;
	
	int i1_out, i2_out, i3_out;
	char tmp_name[128];
	int i;
	int num = count_chara_int3_clist(grp_vws->ci3_clist_gtk);
	for(i=0;i<num;i++){
		set_from_chara_int3_clist_at_index(i, grp_vws->ci3_clist_gtk, 
					tmp_name, &i1_out, &i2_out, &i3_out);
		if(iflag_column == COLUMN_MESH_THIRD){
			update_chara_int3_clist_by_index(i, tmp_name, iflag, i2_out, i3_out, 
						grp_vws->ci3_clist_gtk);
		} else if(iflag_column == COLUMN_MESH_FORTH) {
			update_chara_int3_clist_by_index(i, tmp_name, i1_out, iflag, i3_out, 
						grp_vws->ci3_clist_gtk);
		} else {
			update_chara_int3_clist_by_index(i, tmp_name, i1_out, i2_out, iflag, 
						grp_vws->ci3_clist_gtk);
		};
		
		gtk_tree_model_iter_nth_child(child_model, &iter, NULL, i);
		gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
					iflag_column, (gboolean) iflag, -1);
	};
	return num;
}


int set_all_node_draw_flags(int iflag, gpointer user_data)
{
	struct ci_clist_view *nod_grp_vws = (struct ci_clist_view *) user_data;
    GtkTreeModel *model = gtk_tree_view_get_model(GTK_TREE_VIEW(nod_grp_vws->tree_view));  
	GtkTreeModel *child_model = gtk_tree_model_sort_get_model(GTK_TREE_MODEL_SORT(model));
    GtkTreeIter iter;
	
	int i1_out;
	char tmp_name[128];
	int i;
	int num = count_chara_int_clist(nod_grp_vws->ci_clist_gtk);
	for(i=0;i<num;i++){
		set_from_chara_int_clist_at_index(i, nod_grp_vws->ci_clist_gtk, 
					tmp_name, &i1_out);
		update_chara_int_clist_by_index(i, tmp_name, iflag, nod_grp_vws->ci_clist_gtk);
		
		gtk_tree_model_iter_nth_child(child_model, &iter, NULL, i);
		gtk_list_store_set(GTK_LIST_STORE(child_model), &iter,
					COLUMN_MESH_THIRD, (gboolean) iflag, -1);
	};
	return num;
}


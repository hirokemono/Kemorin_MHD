
#include <string.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include "control_elements_IO_c.h"
#include "t_control_int_IO.h"
#include "t_control_real_IO.h"
#include "t_control_chara_IO.h"

#include "control_elements_IO_GTK.h"
#include "control_combobox_GTK.h"
#include "control_panel_4_field_GTK.h"
#include "t_control_data_4_iso_c.h"
#include "kemoview_gtk_routines.h"
#include "tree_view_chara_GTK.h"
#include "tree_view_4_field_GTK.h"

extern void c_view_control_sph_SGS_MHD();
extern void * c_read_control_sph_SGS_MHD(char *file_name);
extern void * c_MHD_block_name(void *f_MHD_ctl);
extern void * c_MHD_iflag(void *f_MHD_ctl);
extern void * c_MHD_plt(void *f_MHD_ctl);
extern void * c_MHD_org_plt(void *f_MHD_ctl);
extern void * c_MHD_new_plt(void *f_MHD_ctl);
extern void * c_MHD_fname_psph(void *f_MHD_ctl);
extern void * c_MHD_psph_ctl(void *f_MHD_ctl);
extern void * c_MHD_model_ctl(void *f_MHD_ctl);
extern void * c_MHD_smctl_ctl(void *f_MHD_ctl);
extern void * c_MHD_smonitor_ctl(void *f_MHD_ctl);
extern void * c_MHD_nmtr_ctl(void *f_MHD_ctl);

extern void * c_plt_block_name(void *f_plt);
extern void * c_plt_iflag(void *f_plt);
extern void * c_plt_ndomain_ctl(void *f_plt);
extern void * c_plt_num_smp_ctl(void *f_plt);
extern void * c_plt_debug_flag_ctl(void *f_plt);
extern void * c_plt_sph_file_prefix(void *f_plt);
extern void * c_plt_mesh_file_prefix(void *f_plt);
extern void * c_plt_restart_file_prefix(void *f_plt);
extern void * c_plt_field_file_prefix(void *f_plt);
extern void * c_plt_spectr_field_file_prefix(void *f_plt);
extern void * c_plt_coriolis_int_file_name(void *f_plt);
extern void * c_plt_bc_data_file_name_ctl(void *f_plt);
extern void * c_plt_radial_data_file_name_ctl(void *f_plt);
extern void * c_plt_interpolate_sph_to_fem(void *f_plt);
extern void * c_plt_interpolate_fem_to_sph(void *f_plt);
extern void * c_plt_rayleigh_spectr_dir(void *f_plt);
extern void * c_plt_rayleigh_field_dir(void *f_plt);
extern void * c_plt_sph_file_fmt_ctl(void *f_plt);
extern void * c_plt_mesh_file_fmt_ctl(void *f_plt);
extern void * c_plt_restart_file_fmt_ctl(void *f_plt);
extern void * c_plt_field_file_fmt_ctl(void *f_plt);
extern void * c_plt_spectr_field_fmt_ctl(void *f_plt);
extern void * c_plt_itp_file_fmt_ctl(void *f_plt);
extern void * c_plt_coriolis_file_fmt_ctl(void *f_plt);
extern void * c_plt_del_org_data_ctl(void *f_plt);

extern void * c_chara_item_block_name(void *f_plt);
extern void * c_chara_item_iflag(void *f_plt);
extern void * c_chara_item_clength(void *f_plt, int *length);
extern void * c_chara_item_charavalue(void *f_plt);

extern void load_chara_from_c(char *f_plt);

struct f_ctl_chara_item{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	int f_clength[1];
	char * f_charavalue;
};

struct f_platform_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	void * f_ndomain_ctl;
	void * f_num_smp_ctl;
	struct f_ctl_chara_item * f_debug_flag_ctl;
	struct f_ctl_chara_item * f_sph_file_prefix;
	struct f_ctl_chara_item * f_mesh_file_prefix;
	struct f_ctl_chara_item * f_restart_file_prefix;
	struct f_ctl_chara_item * f_field_file_prefix;
	struct f_ctl_chara_item * f_spectr_field_file_prefix;
	struct f_ctl_chara_item * f_coriolis_int_file_name;
	struct f_ctl_chara_item * f_bc_data_file_name_ctl;
	struct f_ctl_chara_item * f_radial_data_file_name_ctl;
	struct f_ctl_chara_item * f_interpolate_sph_to_fem;
	struct f_ctl_chara_item * f_interpolate_fem_to_sph;
	struct f_ctl_chara_item * f_rayleigh_spectr_dir;
	struct f_ctl_chara_item * f_rayleigh_field_dir;
	struct f_ctl_chara_item * f_sph_file_fmt_ctl;
	struct f_ctl_chara_item * f_mesh_file_fmt_ctl;
	struct f_ctl_chara_item * f_restart_file_fmt_ctl;
	struct f_ctl_chara_item * f_field_file_fmt_ctl;
	struct f_ctl_chara_item * f_spectr_field_fmt_ctl;
	struct f_ctl_chara_item * f_itp_file_fmt_ctl;
	struct f_ctl_chara_item * f_coriolis_file_fmt_ctl;
	struct f_ctl_chara_item * f_del_org_data_ctl;
};

struct f_MHD_control{
	void * f_self;
	
	char * f_block_name;
	int * f_iflag;
	
	struct f_platform_control *f_plt;
	void * f_org_plt;
	void * f_new_plt;
	void * f_fname_psph;
	void * f_psph_ctl;
	void * f_model_ctl;
	void * f_smctl_ctl;
	void * f_smonitor_ctl;
	void * f_nmtr_ctl;
};

void draw_MHD_control_list(GtkWidget *window, GtkWidget *vbox0, struct f_MHD_control *f_MHD_ctl, struct iso_ctl_c *iso_c);

int iflag_read_iso = 0;

struct iso_ctl_GTK{
	struct iso_ctl_c *iso_c;
	
	struct field_ctl_c *iso_field_ctl;
	struct field_views *iso_fields_vws;
	
	struct field_ctl_c *color_field_ctl;
	struct field_views *color_fields_vws;
};


struct iso_ctl_GTK *iso_GTK0;


// struct SGS_MHD_control_c *mhd_ctl;
GtkWidget *window;
GtkWidget *vbox_0;

GtkWidget *entry_3, *entry_4, *entry_5;

double rtest = 2.5;
int ntest = 66;
char *ctest = "ahahahaha";
struct chara_ctl_item item_test = {55, "tako_tako"};
struct chara_ctl_item *ptem_test;

void *MHD_ctl_C;

/*
static gboolean
boolean_to_text (GBinding *binding,
                 const GValue *source,
                 GValue *target,
                 gpointer dummy G_GNUC_UNUSED)
{
	if (g_value_get_boolean (source)){
		g_value_set_string (target, "On");
	}else{
		g_value_set_string (target, "Off");
	}
	
	return TRUE;
}
*/

struct f_ctl_chara_item * init_f_ctl_chara_item(void *(*c_load_self)(void *f_self0), void *f_self0)
{
	struct f_ctl_chara_item *f_citem = (struct f_ctl_chara_item *) malloc(sizeof(struct f_ctl_chara_item));
	if(f_citem == NULL){
		printf("malloc error for f_ctl_chara_item\n");
		exit(0);
	};
	f_citem->f_self =  c_load_self(f_self0);
	
	f_citem->f_block_name =  (char *)  c_chara_item_block_name(f_citem->f_self);
	f_citem->f_iflag =        (int *)   c_chara_item_iflag(f_citem->f_self);
	f_citem->f_charavalue =  (char *)  c_chara_item_charavalue(f_citem->f_self);
	c_chara_item_clength(f_citem->f_self, f_citem->f_clength);
	
	printf("f_citem->f_self %p \n", f_citem->f_self);
	printf("f_citem->f_block_name %s \n", f_citem->f_block_name);
	printf("f_citem->f_charavalue %d %s \n", 
		   f_citem->f_iflag[0], f_citem->f_charavalue);
		   
	return f_citem;
}

struct f_platform_control * init_f_platform_control(void *f_self)
{
	struct f_platform_control *f_plt = (struct f_platform_control *) malloc(sizeof(struct f_platform_control));
	if(f_plt == NULL){
		printf("malloc error for f_plt\n");
		exit(0);
	};
	printf("f_self %p \n", f_self);
	f_plt->f_self =  c_MHD_plt(f_self);
	f_plt->f_block_name =  (char *) c_plt_block_name(f_plt->f_self);
	f_plt->f_iflag =       (int *)  c_plt_iflag(f_plt->f_self);
	
	f_plt->f_ndomain_ctl =               c_plt_ndomain_ctl(f_plt->f_self);
	f_plt->f_num_smp_ctl =               c_plt_num_smp_ctl(f_plt->f_self);
	
	
	f_plt->f_debug_flag_ctl =            init_f_ctl_chara_item(c_plt_debug_flag_ctl, f_plt->f_self);
	f_plt->f_sph_file_prefix =           init_f_ctl_chara_item(c_plt_sph_file_prefix, f_plt->f_self);
	f_plt->f_mesh_file_prefix =          init_f_ctl_chara_item(c_plt_mesh_file_prefix, f_plt->f_self);
	f_plt->f_restart_file_prefix =       init_f_ctl_chara_item(c_plt_restart_file_prefix, f_plt->f_self);
	f_plt->f_field_file_prefix =         init_f_ctl_chara_item(c_plt_field_file_prefix, f_plt->f_self);
	f_plt->f_spectr_field_file_prefix =  init_f_ctl_chara_item(c_plt_spectr_field_file_prefix, f_plt->f_self);
	f_plt->f_coriolis_int_file_name =    init_f_ctl_chara_item(c_plt_coriolis_int_file_name, f_plt->f_self);
	f_plt->f_bc_data_file_name_ctl =     init_f_ctl_chara_item(c_plt_bc_data_file_name_ctl, f_plt->f_self);
	f_plt->f_radial_data_file_name_ctl = init_f_ctl_chara_item(c_plt_radial_data_file_name_ctl, f_plt->f_self);
	f_plt->f_interpolate_sph_to_fem =    init_f_ctl_chara_item(c_plt_interpolate_sph_to_fem, f_plt->f_self);
	f_plt->f_interpolate_fem_to_sph =    init_f_ctl_chara_item(c_plt_interpolate_fem_to_sph, f_plt->f_self);
	f_plt->f_rayleigh_spectr_dir =       init_f_ctl_chara_item(c_plt_rayleigh_spectr_dir, f_plt->f_self);
	f_plt->f_rayleigh_field_dir =        init_f_ctl_chara_item(c_plt_rayleigh_field_dir, f_plt->f_self);
	f_plt->f_sph_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_sph_file_fmt_ctl, f_plt->f_self);
	f_plt->f_mesh_file_fmt_ctl =         init_f_ctl_chara_item(c_plt_mesh_file_fmt_ctl, f_plt->f_self);
	f_plt->f_restart_file_fmt_ctl =      init_f_ctl_chara_item(c_plt_restart_file_fmt_ctl, f_plt->f_self);
	f_plt->f_field_file_fmt_ctl =        init_f_ctl_chara_item(c_plt_field_file_fmt_ctl, f_plt->f_self);
	f_plt->f_spectr_field_fmt_ctl =      init_f_ctl_chara_item(c_plt_spectr_field_fmt_ctl, f_plt->f_self);
	f_plt->f_itp_file_fmt_ctl =          init_f_ctl_chara_item(c_plt_itp_file_fmt_ctl, f_plt->f_self);
	f_plt->f_coriolis_file_fmt_ctl =     init_f_ctl_chara_item(c_plt_coriolis_file_fmt_ctl, f_plt->f_self);
	f_plt->f_del_org_data_ctl =          init_f_ctl_chara_item(c_plt_del_org_data_ctl, f_plt->f_self);
	return f_plt;
}

static void cb_View(GtkButton *button, gpointer data)
{
	c_view_control_sph_SGS_MHD();
}
static void cb_Open(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN,
									 GTK_FILE_CHOOSER_ACTION_SAVE,
									 GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
									 GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
  gchar *read_file_name;
  gchar *folder;
	
	
	char buf[LENGTHBUF];      /* character buffer for reading line */
	
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) g_object_get_data(G_OBJECT(data), "MHD_ctl");
	parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
	entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[0],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Open", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);

	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name  */
		read_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		gtk_file_chooser_set_current_folder(GTK_FILE_CHOOSER(dialog),
											read_file_name);
		g_print( "file name: %s\n", read_file_name);
		
		folder = gtk_file_chooser_get_current_folder(GTK_FILE_CHOOSER(dialog));
		if ((folder == NULL)) {
			int length = strlen(read_file_name);
			char *stripped_filehead = (char *) calloc(length+1, sizeof(char));
			char *stripped_dir = (char *) calloc(length+1, sizeof(char));
			split_dir_and_file_name_c((char *) read_file_name, 
									  stripped_dir, stripped_filehead);
			printf("Folder %s\n", stripped_dir);
			chdir(stripped_dir);
		} else {
			g_print( "folder name: %s\n", folder);
			chdir(folder);
		}
		/* Get Folder name */
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
		f_MHD_ctl->f_self = c_read_control_sph_SGS_MHD((char *) read_file_name);
		f_MHD_ctl->f_block_name =   (char *) c_MHD_block_name(f_MHD_ctl->f_self);
		f_MHD_ctl->f_iflag =        (int *) c_MHD_iflag(f_MHD_ctl->f_self);
		
		f_MHD_ctl->f_plt = init_f_platform_control(f_MHD_ctl->f_self);
		
		f_MHD_ctl->f_org_plt =      c_MHD_org_plt(f_MHD_ctl->f_self);
		f_MHD_ctl->f_new_plt =      c_MHD_new_plt(f_MHD_ctl->f_self);
		f_MHD_ctl->f_fname_psph =   c_MHD_fname_psph(f_MHD_ctl->f_self);
		f_MHD_ctl->f_psph_ctl =     c_MHD_psph_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_model_ctl =    c_MHD_model_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_smctl_ctl =    c_MHD_smctl_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_smonitor_ctl = c_MHD_smonitor_ctl(f_MHD_ctl->f_self);
		f_MHD_ctl->f_nmtr_ctl =     c_MHD_nmtr_ctl(f_MHD_ctl->f_self);
		
		/* Show file name in entry */ 
		gtk_entry_set_text(entry, read_file_name);
		
		if((iso_GTK0 = (struct iso_ctl_GTK *) malloc(sizeof(struct iso_ctl_GTK))) == NULL) {
			printf("malloc error for iso_ctl_GTK \n");
			exit(0);
		}
		
		iso_GTK0->iso_c = init_iso_ctl_c();        
		int iflag = read_iso_ctl_file_c(read_file_name, buf, iso_GTK0->iso_c);
		iflag_read_iso = 1;
		g_free(read_file_name);
		printf("iso_output_type_ctl original %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		set_primary_iso_format_flag_c(iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		printf("iso_output_type_ctl modified %s\n", iso_GTK0->iso_c->iso_output_type_ctl->c_tbl);
		
		
		draw_MHD_control_list(window, vbox_0, f_MHD_ctl, iso_GTK0->iso_c);
		gtk_widget_show_all(window);
	}else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	}else{
		g_print( "Another response was received.\n" );
	}
	gtk_widget_destroy(dialog);
}

static void cb_Save(GtkButton *button, gpointer data)
{
  GtkWidget *dialog;
  GtkWidget *parent;
  GtkEntry *entry;
	
  /* Four selections for GtkFileChooserAction */
	GtkFileChooserAction action[] = {GTK_FILE_CHOOSER_ACTION_OPEN, GTK_FILE_CHOOSER_ACTION_SAVE,
			GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER};
  gint response;
	gchar *write_file_name;

  parent = GTK_WIDGET(g_object_get_data(G_OBJECT(data), "parent"));
  entry = GTK_ENTRY(data);

	/* generate file selection widget */
	dialog = gtk_file_chooser_dialog_new("File Chooser Dialog", GTK_WINDOW(parent), action[1],
				"_Cancel", GTK_RESPONSE_CANCEL, "_Save", GTK_RESPONSE_ACCEPT, NULL);
	
	gtk_widget_show_all(dialog);
	
	response = gtk_dialog_run(GTK_DIALOG(dialog));
	if( response == GTK_RESPONSE_ACCEPT ){
		g_print( "File is selecting \n");
		/* Get file name */
		write_file_name = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));
		g_print( "Write file name: %s\n", write_file_name);
		
		gtk_entry_set_text(entry, write_file_name);
		
		write_iso_ctl_file_c(write_file_name, iso_GTK0->iso_c);
		dealloc_iso_ctl_c(iso_GTK0->iso_c);
		free(iso_GTK0);
		g_free(write_file_name);
		iflag_read_iso = 0;
		
	} else if( response == GTK_RESPONSE_CANCEL ){
		g_print( "Cancel button was pressed.\n" );
	} else{
		g_print( "Another response was received.\n" );
	};
	gtk_widget_destroy(dialog);
}

void expander_MHD_ctl_callback(GObject *object, GParamSpec *param_spec, gpointer user_data){
	GtkExpander *expander;

	expander = GTK_EXPANDER (object);
	if (gtk_expander_get_expanded (expander)){
		printf("Expanded \n");
	}else{
		printf("Hided \n");
	}
	gtk_widget_show_all(window);
};

static void cb_edited_egrp(GtkCellRendererText *cell, gchar *path_str, 
                           gchar *new_text, gpointer user_data){
    printf("path_str %s\n", path_str);
    printf("new_text %s\n", new_text);
}

static void cb_add(GtkButton *button, gpointer user_data)
{
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *iso_area_list
			= (struct chara_clist *) g_object_get_data(G_OBJECT(user_data), "chara_list");
	add_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), iso_area_list);
}
static void cb_delete(GtkButton *button, gpointer user_data)
{
	GtkWidget *c_tree_view = GTK_WIDGET(user_data);
	struct chara_clist *iso_area_list
			= (struct chara_clist *) g_object_get_data(G_OBJECT(user_data), "chara_list");
	delete_c_list_items_GTK(GTK_TREE_VIEW(c_tree_view), iso_area_list);
}

GtkWidget * iso_define_ctl_list_box(struct iso_define_ctl_c *iso_def_c){
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_2[2];
	
	iso_GTK0->iso_field_ctl = init_field_ctl_c();
	iso_GTK0->iso_fields_vws = init_field_views_GTK(iso_GTK0->iso_field_ctl);
	iso_GTK0->iso_fields_vws->used_tree_view 
			= create_field_tree_view(iso_GTK0->iso_fields_vws->all_fld_list,
									 iso_GTK0->iso_fields_vws->fld_ctl_gtk);
	iso_GTK0->iso_fields_vws->unused_field_tree_view
			= create_unused_field_tree_views(iso_GTK0->iso_fields_vws->all_fld_list);
	
	iso_GTK0->iso_fields_vws->field_group_tree_view
			= create_field_group_tree_view(iso_GTK0->iso_fields_vws->all_fld_list);
	iso_GTK0->iso_fields_vws->all_field_tree_view
			= create_all_field_tree_views(iso_GTK0->iso_fields_vws->all_fld_list);
	iso_GTK0->iso_fields_vws->selected_field_ctl =     iso_def_c->isosurf_data_ctl;
	iso_GTK0->iso_fields_vws->selected_component_ctl = iso_def_c->isosurf_comp_ctl;
	
	create_direction_tree_views(iso_GTK0->iso_fields_vws);
	
	printf("isosurf_data_ctl: %s\n", iso_def_c->isosurf_data_ctl->c_tbl);
	printf("isosurf_comp_ctl: %s\n", iso_def_c->isosurf_comp_ctl->c_tbl);
	add_all_field_combobox_vbox("Field_ctl:", "Comp_ctl:", 
								iso_GTK0->iso_fields_vws, vbox_1);
	
	vbox_2[0] = make_real_hbox(1, iso_def_c->label_iso_define_ctl->label[ 2],
							   iso_def_c->isosurf_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	
	printf("iso_area_list %d\n", count_chara_clist(iso_def_c->iso_area_list));
	int index = 0;
	GtkWidget *c_tree_view = gtk_tree_view_new();
    GtkCellRenderer *renderer_text = gtk_cell_renderer_text_new();

    create_text_tree_view(c_tree_view, renderer_text, iso_def_c->iso_area_list);
    index = append_c_list_from_ctl(index, &iso_def_c->iso_area_list->c_item_head, c_tree_view);
    g_signal_connect(G_OBJECT(renderer_text), "edited", 
                     G_CALLBACK(cb_edited_egrp), (gpointer) iso_def_c);
    
    
	printf("index %d\n", index);
	GtkWidget *button_A = gtk_button_new_with_label("Add");
	GtkWidget *button_D = gtk_button_new_with_label("Delete");
	g_object_set_data(G_OBJECT(c_tree_view), "chara_list",
					  (gpointer) iso_def_c->iso_area_list);
	g_signal_connect(G_OBJECT(button_A), "clicked", G_CALLBACK(cb_add),
					 (gpointer) c_tree_view);
	g_signal_connect(G_OBJECT(button_D), "clicked", G_CALLBACK(cb_delete),
					 (gpointer) c_tree_view);
	add_chara_list_box_w_addbottun(c_tree_view, 
								   button_A, button_D, vbox_1);
	return vbox_1;
};


GtkWidget * iso_field_ctl_list_box(struct iso_field_ctl_c *iso_fld_c){
	GtkWidget *vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *vbox_2[2];
	
	char *c_label;
	
	iso_GTK0->color_field_ctl = init_field_ctl_c();
	iso_GTK0->color_fields_vws = init_field_views_GTK(iso_GTK0->color_field_ctl);
	iso_GTK0->color_fields_vws->used_tree_view 
			= create_field_tree_view(iso_GTK0->color_fields_vws->all_fld_list,
									 iso_GTK0->color_fields_vws->fld_ctl_gtk);
	iso_GTK0->color_fields_vws->unused_field_tree_view
			= create_unused_field_tree_views(iso_GTK0->color_fields_vws->all_fld_list);
	
	iso_GTK0->color_fields_vws->field_group_tree_view
			= create_field_group_tree_view(iso_GTK0->color_fields_vws->all_fld_list);
	iso_GTK0->color_fields_vws->all_field_tree_view
			= create_all_field_tree_views(iso_GTK0->color_fields_vws->all_fld_list);
	create_direction_tree_views(iso_GTK0->color_fields_vws);
	
	
	GtkWidget *color_flags_tree_view
			= create_control_flags_tree_view(iso_fld_c->flag_iso_color);
	
	add_control_combobox_vbox(iso_fld_c->label_fld_on_iso_ctl->label[ 0], 
							  iso_fld_c->output_type_ctl->c_tbl, 
							  iso_fld_c->flag_iso_color, 
							  color_flags_tree_view, vbox_1);
	printf("%le\n", iso_fld_c->output_value_ctl->r_data);
	c_label = duplicate_underscore(iso_fld_c->label_fld_on_iso_ctl->label[ 2]);
	vbox_2[0] = make_real_hbox(1, c_label, iso_fld_c->output_value_ctl);
	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_field_selection_box(iso_GTK0->color_fields_vws, window, vbox_1);
	
	return vbox_1;
};

static void cb_check_toggle(GtkWidget *widget, gpointer iflag_ptr){
	int *iflag_block = (int *) iflag_ptr;
	
	if(gtk_toggle_button_get_active(widget) == TRUE){
		*iflag_block = 1;
	}else{
		*iflag_block = 0;
	};
	printf("*iflag_block %d \n", *iflag_block);
	return;
}

static void cb_chara_ctl_item(GtkEntry *entry, gpointer data)
{
	struct f_ctl_chara_item *f_citem = (struct chara_ctl_item *) data;
	gchar * input_text;
	
	if(f_citem->f_self != NULL) {
/*		f_citem->f_iflag[0] = 1; */
		input_text = gtk_entry_get_text(entry);
		f_citem->f_clength[0] = strngcopy(f_citem->f_charavalue, (char *) input_text);
		load_chara_from_c(f_citem->f_charavalue);
	};
	return;
}


GtkWidget * draw_chara_item_entry_box(struct f_ctl_chara_item * f_citem, GtkWidget *window)
{
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	if(f_citem->f_iflag[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) f_citem->f_iflag);
	GtkWidget *label = gtk_label_new(duplicate_underscore(f_citem->f_block_name));
	
	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_entry_set_max_width_chars(entry, 80);
	gtk_entry_set_text(entry, f_citem->f_charavalue);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer) window);
	g_signal_connect(G_OBJECT(entry), "activate", G_CALLBACK(cb_chara_ctl_item), 
					 (gpointer) f_citem);
	
	gtk_box_pack_start(GTK_BOX(hbox), checkbox, TRUE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	return hbox;
}

GtkWidget * draw_control_block(const char * title, int *iflag_ptr, 
							   int width, int height,
							   GtkWidget *window, GtkWidget *box_in)
{
	GtkWidget *vbox0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	GtkWidget *hbox0 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *checkbox = gtk_check_button_new();
	if(iflag_ptr[0] == 0){
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), FALSE);
	} else {
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(checkbox), TRUE);
	}
	g_signal_connect(G_OBJECT(checkbox), "toggled", 
                     G_CALLBACK(cb_check_toggle), (gpointer) iflag_ptr);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(title), width, height, window, box_in);
	gtk_box_pack_start(GTK_BOX(vbox0), checkbox, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), vbox0, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(hbox0), expander, FALSE, FALSE, 0);
	return hbox0;
};



void draw_MHD_control_list(GtkWidget *window, GtkWidget *vbox0, struct f_MHD_control *f_MHD_ctl, struct iso_ctl_c *iso_c){
	GtkWidget *vbox_1;
	GtkWidget *vbox_2[iso_c->label_iso_ctl_w_dpl->num_labels];
	
	char *c_label;
	
	GtkWidget *file_fmt_flags_tree_view
			= create_control_flags_tree_view(iso_c->flag_iso_format);
	
	/* Generate expander */
	vbox_1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	
	vbox_2[0] = make_text_hbox
    		(0, duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 0]),
             iso_c->iso_file_head_ctl);

	gtk_box_pack_start(GTK_BOX(vbox_1), vbox_2[0], FALSE, FALSE, 0);
	add_control_combobox_vbox(iso_c->label_iso_ctl_w_dpl->label[ 1], 
							  iso_c->iso_output_type_ctl->c_tbl,
							  iso_c->flag_iso_format, 
							  file_fmt_flags_tree_view, vbox_1);
	
	vbox_2[2] = iso_define_ctl_list_box(iso_c->iso_def_c);
	vbox_2[3] = iso_field_ctl_list_box(iso_c->iso_fld_c);
	
	GtkWidget *expander = wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 2]), 
			 400, 200, window, vbox_2[2]);
	
	GtkWidget *hbox_c1 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_debug_flag_ctl, window);
	GtkWidget *hbox_c2 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_sph_file_prefix, window);
	GtkWidget *hbox_c3 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_sph_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c4 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_mesh_file_prefix, window);
	GtkWidget *hbox_c5 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_mesh_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c6 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_restart_file_prefix, window);
	GtkWidget *hbox_c7 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_restart_file_fmt_ctl, window);
	GtkWidget *hbox_c8 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_field_file_prefix, window);
	GtkWidget *hbox_c9 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_field_file_fmt_ctl, window);
	/*
	GtkWidget *hbox_c10 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_spectr_field_file_prefix, window);
	GtkWidget *hbox_c11 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_spectr_field_fmt_ctl, window);
	GtkWidget *hbox_c12 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_coriolis_int_file_name, window);
	GtkWidget *hbox_c13 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_coriolis_int_file_name, window);
	*/
	GtkWidget *hbox_c14 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_coriolis_file_fmt_ctl, window);
	GtkWidget *hbox_c15 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_radial_data_file_name_ctl, window);
	/*
	GtkWidget *hbox_c16 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_interpolate_sph_to_fem, window);
	GtkWidget *hbox_c17 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_interpolate_fem_to_sph, window);
	GtkWidget *hbox_c18 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_itp_file_fmt_ctl, window);
	*/
	GtkWidget *hbox_c19 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_rayleigh_spectr_dir, window);
	GtkWidget *hbox_c20 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_rayleigh_field_dir, window);
	
	GtkWidget *hbox_c21 = draw_chara_item_entry_box(f_MHD_ctl->f_plt->f_del_org_data_ctl, window);
	
	GtkWidget *vbox_plt = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c1, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c2, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c3, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c6, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c7, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c8, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c9, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c14, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c15, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c19, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c20, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_plt), hbox_c21, FALSE, FALSE, 0);
	
    gtk_box_pack_start(GTK_BOX(vbox_1), expander, FALSE, FALSE, 0);
	GtkWidget *expander1 = wrap_into_expanded_frame_gtk
			(duplicate_underscore(iso_c->label_iso_ctl_w_dpl->label[ 3]), 
			 400, 500, window, vbox_2[3]);
    gtk_box_pack_start(GTK_BOX(vbox_1), vbox_plt, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(vbox_1), expander1, FALSE, FALSE, 0);
	c_label = isosurface_control_head();
    
    GtkWidget *evo_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
	GtkWidget *expander2 = wrap_into_expanded_frame_gtk(duplicate_underscore(c_label),
								 400, 600, window, vbox_1);
	gtk_box_pack_start(GTK_BOX(evo_box), expander2, FALSE, FALSE, 0);
	
	int iflag_ptr[1];
	iflag_ptr[0] = 0;
	GtkWidget *expand_PLT = draw_control_block(f_MHD_ctl->f_plt->f_block_name, 
											   f_MHD_ctl->f_plt->f_iflag,
											   360, 240, window, evo_box);
	GtkWidget *expand_MHD = draw_control_block(f_MHD_ctl->f_block_name, 
											   f_MHD_ctl->f_iflag,
											   360, 240, window, expand_PLT);
	gtk_box_pack_start(GTK_BOX(vbox0), expand_MHD, FALSE, FALSE, 0);
	return;
};


void draw_MHD_control_bottuns(GtkWidget *vbox0){
	struct f_MHD_control *f_MHD_ctl = (struct f_MHD_control *) malloc(sizeof(struct f_MHD_control));
		printf("f_MHD_ctl %p\n", f_MHD_ctl);
	if(f_MHD_ctl == NULL){
		printf("malloc error for f_MHD_ctl\n");
		exit(0);
	};
	
	GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
	GtkWidget *label = gtk_label_new("File:");
	gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, FALSE, 0);

	/* Generate file entry  */
	GtkWidget *entry = gtk_entry_new();
	gtk_box_pack_start(GTK_BOX(hbox), entry, TRUE, TRUE, 0);
	g_object_set_data(G_OBJECT(entry), "parent", (gpointer)window);
	g_object_set_data(G_OBJECT(entry), "MHD_ctl", (gpointer)f_MHD_ctl);
	
	/* Generate Bottuns */
	GtkWidget *button_O = gtk_button_new_with_label("Open");
	GtkWidget *button_V = gtk_button_new_with_label("View");
	GtkWidget *button_S = gtk_button_new_with_label("Save");
	GtkWidget *button_Q = gtk_button_new_with_label("Quit");
	
	
	
	g_signal_connect(G_OBJECT(button_O), "clicked", G_CALLBACK(cb_Open), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_V), "clicked", G_CALLBACK(cb_View), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_S), "clicked", G_CALLBACK(cb_Save), (gpointer)entry);
	g_signal_connect(G_OBJECT(button_Q), "clicked", G_CALLBACK(gtk_main_quit), NULL);
	
	gtk_box_pack_start(GTK_BOX(hbox), button_O, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_V, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_S, FALSE, FALSE, 0);
	gtk_box_pack_start(GTK_BOX(hbox), button_Q, FALSE, FALSE, 0);
	
	gtk_box_pack_start(GTK_BOX(vbox0), hbox, FALSE, FALSE, 0);
	
}



int main(int argc, char** argv)
{
	gtk_init(&argc, &argv);
	
	window =gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(window), "FileChooser");
	gtk_container_set_border_width(GTK_CONTAINER(window), 5);
	g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(gtk_main_quit), NULL);
	
	
//	GtkWidget *scroll_window = gtk_scrolled_window_new(NULL, NULL);
//	gtk_box_pack_start(GTK_BOX(vbox_0), scroll_window, TRUE, TRUE, 0);
	
			ptem_test = init_chara_ctl_item_c();
			ptem_test->iflag = 111;
			ptem_test->c_tbl = "gggg";
	
	vbox_0 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
	draw_MHD_control_bottuns(vbox_0);
	gtk_container_add(GTK_CONTAINER(window), vbox_0);
	
	gtk_widget_show_all(window);
	gtk_main();

	return 0;
}

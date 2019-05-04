/*
 *  kemoview_fileselector_glui.cpp
 *  Kemoview_Cocoa
 *
 *  Created by Hiroaki Matsui on 12/03/04.
 *  Copyright 2012 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
 *
 */

#include "kemoview_fileselector_glui.h"

static GLUI *glui_fwin;

static GLUI_TextBox    *currentDir;
static GLUI_String text_current;

static GLUI_FileBrowser *file_brouser;
static GLUI_EditText   *editText_filename;
static GLUI_String text_fname;

static GLUI_StaticText   *staticText_p;
static GLUI_EditText   *editText_st;
static GLUI_EditText   *editText_ed;
static GLUI_EditText   *editText_ic;
static GLUI_Button *bottunToGo;

static GLUI_RadioGroup *imgfmt_radio;
static GLUI_String glui_name;

struct kv_string *kv_filename;
struct kv_string *image_prefix;
static int int_box;
static int image_fmt;
static int ist_udt, ied_udt, inc_udt;

static int counter = 0;



static void save_psf_colormap_glui(int sel){
    int length;
	char current[LENGTHBUF];
    struct kv_string *filename = kemoview_alloc_kvstring();
	
	getcwd(current, sizeof(current));
    length = strlen(current) + strlen(text_fname.c_str()) + 5;
	
    kemoview_alloc_kvstringitem(length, filename);
	strcpy(filename->string, current);
	strcat(filename->string, "/");
	strcat(filename->string, text_fname.c_str());
	
	kemoview_write_PSF_colormap_file(filename);
    kemoview_free_kvstring(filename);
	GLUI_Master.close_all();
	return;
};

static void save_viewmatrix_glui(int sel){
    int length;
    struct kv_string *filename;
	char current[LENGTHBUF];
	
	getcwd(current, sizeof(current));
    length = strlen(current) + strlen(text_fname.c_str()) + 5;
	
    kemoview_alloc_kvstringitem(length, filename);
	strcpy(filename->string, current);
	strcat(filename->string, "/");
	strcat(filename->string, text_fname.c_str());
	
	kemoview_write_modelview_file(filename);
    kemoview_free_kvstring(filename);
	GLUI_Master.close_all();
	return;
};


/* Actions for file name input */
/*
void myGlutIdle( void )
{
	counter++;
	if(counter >= 100000) counter = 0;
};
*/
void reset_glui_idle_count(){
	counter = 0;
	return;
};

int send_glui_idle_count(){return counter;};

static void SetFilenameCB(int val)
{
	glui_name = editText_filename->get_text();
	glui_fwin->sync_live();
}

static void NoSetFilenameCB(int val)
{
	glui_fwin->sync_live();
}

static void fileBrowerCB(int val)
{
	char current[LENGTHBUF];
	getcwd(current, sizeof(current));
	
	text_fname = file_brouser->get_file();
	text_current = current;
	
	currentDir->update_and_draw_text();
	glui_fwin->sync_live();
	
    kv_filename = kemoview_init_kvstring_by_string(text_fname.c_str());
}

static void input_image_file_panel(int val)
{
	text_fname = editText_filename->get_text();
    kv_filename = kemoview_init_kvstring_by_string(text_fname.c_str());
	glui_fwin->sync_live();
	return;
}

static void imgfmt_list_CB( int int_CV )
{
	printf( "list, imege_fmt: %d\n", image_fmt);
	return;
}

static void imgfmt_CB( int int_CV )
{
	int_box = imgfmt_radio->get_int_val();
	
	if     (int_box == 1) {image_fmt = SAVE_PNG;}
	else if(int_box == 2) {image_fmt = SAVE_BMP;}
	else if(int_box == 3) {image_fmt = SAVE_EPS;}
	else if(int_box == 4) {image_fmt = SAVE_PDF;}
	else if(int_box == 5) {image_fmt = SAVE_PS;}
	else {image_fmt = NO_SAVE_FILE;};
	
	/*	printf( "checkbox, imege_fmt: %d %d\n", int_box, image_fmt);*/
	return;
}

static void input_evolution_start_panel(int val){
	int istep = editText_st->get_int_val();
	if(istep <= ied_udt) ist_udt = istep;
	glui_fwin->sync_live();
	return;
}
static void input_evolution_end_panel(int val){
	int istep = editText_ed->get_int_val();
	if(istep >= ist_udt) ied_udt = istep;
	glui_fwin->sync_live();
	return;
}
static void input_evolution_increment_panel(int val){
	int istep = editText_ic->get_int_val();
	if(istep >= 0) inc_udt = istep;
	glui_fwin->sync_live();
	return;
}

static void save_image_handler(int sel){
	draw_mesh_keep_menu();
    kemoview_write_window_to_file(image_fmt, image_prefix);
    kemoview_free_kvstring(image_prefix);
	GLUI_Master.close_all();
	return;
};

static void save_evolution_handler(int sel){
	write_evolution_views_glut(image_fmt, image_prefix, ist_udt, ied_udt, inc_udt);
    kemoview_free_kvstring(image_prefix);
	GLUI_Master.close_all();
	return;
};


static void save_rotation_views_handler(int rot_dir){
	write_rotate_views_glut(image_fmt, image_prefix, rot_dir);
    kemoview_free_kvstring(image_prefix);
	GLUI_Master.close_all();
};

/* Routines for GLUI tools */

static void set_imagefile_brouser_glui(){
	currentDir = new GLUI_TextBox(glui_fwin, text_current, false, -2);
	editText_filename = new GLUI_EditText( glui_fwin, "File name:", text_fname,
										-1, input_image_file_panel);
	file_brouser = new GLUI_FileBrowser(glui_fwin, "File list", GLUI_PANEL_RAISED,
			0, fileBrowerCB);
	editText_filename->set_w(300);
	currentDir->set_w(300);
	currentDir->set_h(20);
	currentDir->disable();
	file_brouser->set_w(240);
}

static void set_imageformat_listbox_glui(){
	GLUI_Listbox *imgfmt_list = new GLUI_Listbox( glui_fwin, "Image format", 
			&image_fmt);
/*			&image_fmt, 4, imgfmt_list_CB);*/
	imgfmt_list->add_item(NO_SAVE_FILE, "No File" );
	imgfmt_list->add_item(SAVE_PNG, "PNG" );
	imgfmt_list->add_item(SAVE_BMP, "BMP" );
	imgfmt_list->add_item(SAVE_EPS, "EPS" );
	imgfmt_list->add_item(SAVE_PDF, "PDF" );
	imgfmt_list->add_item(SAVE_PS, "PS" );
}

/*static void set_imageformat_buttuns_glui(){
	GLUI_Panel *imgfmt_panel = new GLUI_Panel( glui_fwin, "Image format" );
	imgfmt_radio = new GLUI_RadioGroup(imgfmt_panel, &int_box,4, imgfmt_CB );
	new GLUI_RadioButton( imgfmt_radio, "No File" );
	new GLUI_RadioButton( imgfmt_radio, "PNG" );
	new GLUI_RadioButton( imgfmt_radio, "BMP" );
	new GLUI_RadioButton( imgfmt_radio, "EPS" );
	new GLUI_RadioButton( imgfmt_radio, "PDF" );
	new GLUI_RadioButton( imgfmt_radio, "PS" );
}
*/

static void set_evolution_panel_glui(){
	editText_st = new GLUI_EditText( glui_fwin, "Start step:	", GLUI_EDITTEXT_INT,
									&ist_udt, -1, input_evolution_start_panel );
	editText_ed = new GLUI_EditText( glui_fwin, "End step:	", GLUI_EDITTEXT_INT,
									&ied_udt, -1, input_evolution_end_panel );
	editText_ic = new GLUI_EditText( glui_fwin, "Increment:	", GLUI_EDITTEXT_INT,
									&inc_udt, -1, input_evolution_increment_panel );
}

/* Panels for file name input */

void set_saveimage_menu_glui(int winid){
	char current[LENGTHBUF];
	getcwd(current, sizeof(current));
	text_current = current;
	int iflag, istep;
	
	int_box = 0;
    image_prefix = kemoview_alloc_kvstring();
	istep = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
	
	glui_fwin = GLUI_Master.create_glui("Set file name for image", 0, 100, 100);
	set_imagefile_brouser_glui();
	set_imageformat_listbox_glui();
	glui_fwin->add_button("Save!", 0, save_image_handler);

	glutSetWindow(winid);
	glui_fwin->set_main_gfx_window(winid);
	return;
}

void set_evolution_menu_glui(int winid){
	char current[LENGTHBUF];
	getcwd(current, sizeof(current));
	text_current = current;
	int iflag;
	
	int_box = 0;
    image_prefix = kemoview_alloc_kvstring();
	ist_udt = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
	ied_udt = ist_udt;
	inc_udt = 1;
	
	glui_fwin = GLUI_Master.create_glui("Evolution Parameter", 0, 100, 100);
	set_imagefile_brouser_glui();
	set_evolution_panel_glui();
	set_imageformat_listbox_glui();
	glui_fwin->add_button("Go!", 0, save_evolution_handler);

	glutSetWindow(winid);
	glui_fwin->set_main_gfx_window(winid);
	return;
}

void set_rotateimages_menu_glui(int winid){
	char current[LENGTHBUF];
	getcwd(current, sizeof(current));
	text_current = current;
	int iflag, istep;
	
	int_box = 0;
    image_prefix = kemoview_alloc_kvstring();
	istep = kemoview_get_PSF_full_path_file_prefix(image_prefix, &iflag);
	
	glui_fwin = GLUI_Master.create_glui("Set file name for image", 0, 100, 100);
	set_imagefile_brouser_glui();
	set_imageformat_listbox_glui();
	glui_fwin->add_button("Rotate X-axis!", IONE, save_rotation_views_handler);
	glui_fwin->add_button("Rotate Y-axis!", ITWO, save_rotation_views_handler);
	glui_fwin->add_button("Rotate Z-axis!", ITHREE, save_rotation_views_handler);
	
	
	glutSetWindow(winid);
	glui_fwin->set_main_gfx_window(winid);
	return;
}


void write_PSF_colormap_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui_fwin = GLUI_Master.create_glui("Save colormap", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui_fwin, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui_fwin, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui_fwin, "Select file", GLUI_PANEL_RAISED, 
										0,fileBrowerCB);
	glui_fwin->add_button("Save", 0, save_psf_colormap_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	
	glutSetWindow(winid);
	glui_fwin->set_main_gfx_window(winid);
	return;
}


void save_viewmatrix_file_glui(int winid){
	char current[LENGTHBUF];
	if(getcwd(current, sizeof(current)) != NULL){
		printf("current dir is %s\n", current);
	}
	text_current = current;
	glui_fwin = GLUI_Master.create_glui("Save view matrix", 0, 100, 100);
	currentDir = new GLUI_TextBox(glui_fwin, text_current, false, -1);
	editText_filename = new GLUI_EditText( glui_fwin, "File name: ", text_fname,
										  -1, SetFilenameCB);
	file_brouser = new GLUI_FileBrowser(glui_fwin, "Select file", GLUI_PANEL_RAISED, 
										0,fileBrowerCB);
	glui_fwin->add_button("Save", 0, save_viewmatrix_glui);
	
	editText_filename->set_w(240);
	file_brouser->set_w(240);
	currentDir->set_w(240);
	currentDir->set_h(20);
	currentDir->disable();
	
	
	glutSetWindow(winid);
	glui_fwin->set_main_gfx_window(winid);
	
	return;
}

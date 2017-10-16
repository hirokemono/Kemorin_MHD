//
//  Colormap_EditorAppDelegate.m
//  Colormap_Editor
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "Colormap_EditorAppDelegate.h"
#include "m_kemoviewer_structure.h"
#include "m_kemoviewer_menu.h"

struct viewer_mesh       *vmesh;
struct psf_data          *psf_d;
struct psf_data          *fline_d;
struct mesh_menu_val     *mesh_menu;
struct psf_menu_val      *psf_menu;
struct fline_menu_val    *fline_menu;
struct view_element      *view_s;
struct glut_menu_address *glut_menu_id;

@implementation Colormap_EditorAppDelegate

@synthesize window;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Insert code here to initialize your application 
}

- (void) init
{
}

@end

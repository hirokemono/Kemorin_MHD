//
//  KemoViewerOpenGLView.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoViewerOpenGLView.h"

@implementation KemoViewerOpenGLView
// ---------------------------------

- (void) swapbuffer_cocoa{
	[_context makeCurrentContext];
	[_context flushBuffer];
	[self setNeedsDisplay:YES];
}

// ---------------------------------
-(void) UpdateImage
{
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
    kemoview_full_modify_view();
	[_resetview UpdateParameters];

	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
    return;
}

-(void) QuickUpdateImage
{
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
    kemoview_quick_viewmatrix();
    kemoview_mono_view();
	[_resetview UpdateParameters];
	
	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
}
// ---------------------------------



// ---------------------------------

// set initial OpenGL state (current context is set)
// called after context is created
- (void) prepareKemoOpenGL
{
	float BgColor4f[4];

	static const NSOpenGLPixelFormatAttribute attr[]= {
			NSOpenGLPFAOpenGLProfile, NSOpenGLProfileVersion3_2Core,
			NSOpenGLPFANoRecovery,
			NSOpenGLPFADoubleBuffer,
			//NSOpenGLPFAWindow,
			NSOpenGLPFAAccelerated,
			NSOpenGLPFAColorSize,    24,
			NSOpenGLPFAAlphaSize,     8,
			NSOpenGLPFADepthSize,    24,
			NSOpenGLPFAStencilSize,   8,
			NSOpenGLPFAMultisample,
//			NSOpenGLPFASampleBuffers, 1,
//			NSOpenGLPFASamples, 4, 
			0
	};
	NSOpenGLPixelFormat *pixelFormat = [[[NSOpenGLPixelFormat alloc] initWithAttributes:attr] autorelease];

	_context = [ self openGLContext ];
	[_context initWithFormat: pixelFormat shareContext: nil];
	[_context makeCurrentContext];

	fprintf(
			stdout,
			"INFO: OpenGL Version: %s\n",
			glGetString(GL_VERSION)
			);
	
	// init GL stuff here
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
	
	glEnable(GL_CULL_FACE);
	glFrontFace(GL_CCW);
	glPolygonOffset (1.0f, 1.0f);
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	kemoview_set_background_color(BgColor4f);

}

// ---------------------------------

- (void) awakeFromNib
{
    id_window = kemoview_get_current_viewer_id();
    kemoview_set_single_viewer_id(id_window);

	[self prepareKemoOpenGL];
	
	kemoviewer_reset_to_init_angle();
	kemoview_init_lighting();

    [NSColor setIgnoresAlpha:NO];
}

@end

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


// set initial OpenGL state (current context is set)
// called after context is created
- (void) awakeFromNib
{
    id_window = kemoview_get_current_viewer_id();
    kemoview_set_single_viewer_id(id_window);

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
    [NSColor setIgnoresAlpha:NO];
}
@end

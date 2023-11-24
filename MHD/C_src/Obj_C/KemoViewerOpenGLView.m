//
//  KemoViewerOpenGLView.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoViewerOpenGLView.h"

// ==================================

int XpixelGLWindow, YpixelGLWindow;
int XpixelRectView, YpixelRectView;

// single set of interaction flags and states
double gDollyPanStartPoint[2] = {0.0, 0.0};
GLboolean gDolly =     FALSE;
GLboolean gPan =       FALSE;
GLboolean gTrackball = FALSE;
KemoViewerOpenGLView * gTrackingViewInfo = NULL;

@implementation KemoViewerOpenGLView
// ---------------------------------

// get number of pixels for OpenGL Window
- (int) getViewSize
{
    int iflag_updated = 0;
    
//	NSRect rectView = [self bounds];
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    
	// ensure camera knows size changed
	if ((YpixelGLWindow != rectView.size.height) ||
	    (XpixelGLWindow != rectView.size.width)) {
        YpixelGLWindow = rectView.size.height;
        XpixelGLWindow = rectView.size.width;
        
//        printf("Pixel size %d, %d\n",XpixelGLWindow, YpixelGLWindow);
        
        iflag_updated = 1;
    }
    return iflag_updated;
}

// ---------------------------------

// handles resizing of GL need context update and if the window dimensions change, a
// a window dimension update, reseting of viewport and an update of the projection matrix
- (void) resizeGL
{
	// ensure camera knows size changed
    int iflag_updated = [self getViewSize];
	if (iflag_updated != 0) {
        iflag_resize = 1;
        reftime_msg = CFAbsoluteTimeGetCurrent(); //reset time in all cases
		kemoview_update_projection_by_viewer_size(XpixelGLWindow, YpixelGLWindow,
                                                  XpixelRectView, YpixelRectView);
        kemoview_set_message_opacity(1.0);
        [_context makeCurrentContext];
		
		[self swapbuffer_cocoa];
    } else{
	    kemoview_set_message_opacity(message_opacity);
    }
}

- (void) swapbuffer_cocoa{
	[_context makeCurrentContext];
	[_context flushBuffer];
	[self setNeedsDisplay:YES];
}

// ---------------------------------
-(void) UpdateImage
{
	[self resizeGL]; // forces projection matrix update (does test for size changes)
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
    iflag_fast = kemoview_quick_viewmatrix();
    kemoview_mono_view();
	[_resetview UpdateParameters];
	
	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
    reftime_quick = CFAbsoluteTimeGetCurrent (); //reset time in all cases
}
// ---------------------------------


- (void)fullDrawTimer:(NSTimer *)timer
{
//    printf("fullDrawTimer on %f %d\n",  CFAbsoluteTimeGetCurrent(), iflag_fast);
    if(iflag_fast == 0) return;
    
    CFTimeInterval deltaTime = CFAbsoluteTimeGetCurrent() - reftime_quick;
    if(deltaTime > 2.0){ // skip pauses
        [self UpdateImage];
        reftime_quick = CFAbsoluteTimeGetCurrent(); //reset time in all cases
        iflag_fast = 0;
    };
    return;
}

- (void)messageTimer:(NSTimer *)timer
{
    if(iflag_resize == 0) return;

    CFTimeInterval deltaTime = CFAbsoluteTimeGetCurrent() - reftime_msg;
    if(deltaTime < 4.5){ // skip pauses
        message_opacity = log10f(10.0 - 2.0*deltaTime);
        [self UpdateImage];
    } else {
        iflag_resize = 0;
        message_opacity = 0.0;
        [self UpdateImage];
    }
    return;
}


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
	
	int iflag_updates = [self getViewSize];
    
    if(iflag_updates != 0){
        kemoview_set_windowsize(XpixelGLWindow, YpixelGLWindow, 
                                XpixelRectView, YpixelRectView);
    };
		
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
	
    iflag_fast = 0;
    reftime_quick = CFAbsoluteTimeGetCurrent ();  // set animation time start time
    timer_quick = [NSTimer timerWithTimeInterval:(1.0f/1.0f) target:self selector:@selector(fullDrawTimer:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:timer_quick forMode:NSDefaultRunLoopMode];
    [[NSRunLoop currentRunLoop] addTimer:timer_quick forMode:NSEventTrackingRunLoopMode]; // ensure timer

    iflag_resize = 0;
    message_opacity = 0.0;
    reftime_msg = CFAbsoluteTimeGetCurrent ();  // set animation time start time
    timer_msg = [NSTimer timerWithTimeInterval:(1.0f/2.0f) target:self selector:@selector(messageTimer:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:timer_msg forMode:NSDefaultRunLoopMode];
    [[NSRunLoop currentRunLoop] addTimer:timer_msg forMode:NSEventTrackingRunLoopMode]; // ensure timer fires during resize
 }

@end

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

- (int) KemoviewHorizontalViewSize
{
    return XpixelGLWindow;
}

- (int) KemoviewVerticalViewSize
{
    return YpixelGLWindow;
}

// update the projection matrix based on camera and view info
- (void) updateProjection
{
    [_context makeCurrentContext];
	// set projection
	kemoview_update_distance();
}

// ---------------------------------

// updates the contexts model view matrix for object and camera moves
- (void) updateModelView
{
    [_context makeCurrentContext];
	
	// move view
    kemoview_modify_view();
	kemoview_reset_animation();
	[_resetview UpdateParameters];
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
-(id) DrawRotation: (NSInteger) int_degree : (NSInteger)rotationaxis
{
	kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis);
	kemoview_set_view_integer(ISET_ROTATE_INCREMENT, (int) int_degree);
    kemoview_set_single_viewer_id(id_window);
    kemoview_modify_view();
	
	[self swapbuffer_cocoa];
	return self;
}

-(id) DrawQuilt: (NSInteger) int_degree : (NSInteger)rotationaxis
{
    kemoview_set_single_viewer_id(id_window);
    kemoview_set_view_integer(ISET_ROTATE_AXIS, (int) rotationaxis);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, int_degree);
    kemoview_quilt();
    
    [self swapbuffer_cocoa];
    return self;
}


-(id) DrawEvolution:(NSInteger)timeStep
{	
    kemoview_set_single_viewer_id(id_window);
	kemoview_viewer_evolution((int) timeStep);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
	kemoview_modify_view();
	
	[self swapbuffer_cocoa];
	return self;
}

-(void) UpdateImage
{
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
    kemoview_set_view_integer(ISET_ROTATE_INCREMENT, IZERO);
	kemoview_modify_view();
	[_resetview UpdateParameters];

	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
    return;
}

-(void) QuickUpdateImage
{
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[_context makeCurrentContext];
	
	// move view
	kemoview_set_single_viewer_id(id_window);
    iflag_fast = kemoview_quick_view();
	[_resetview UpdateParameters];
	
	[self swapbuffer_cocoa];
	[self setNeedsDisplay: YES];
    reftime_quick = CFAbsoluteTimeGetCurrent (); //reset time in all cases
}

- (void) drawRectforError:(NSRect)rect
{		
	// setup viewport and prespective
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[self updateModelView];  // update model view matrix for object
	[self swapbuffer_cocoa];
}

// ---------------------------------

// per-window timer function, basic time based animation preformed here
- (void)animationTimer:(NSTimer *)timer
{
    /*
	BOOL shouldDraw = NO;
	if (fAnimate) {
        CFTimeInterval currentTime = CFAbsoluteTimeGetCurrent ();
		CFTimeInterval deltaTime = currentTime - time_anime;
		if (deltaTime > 10.0) // skip pauses
			return;
		else {
			// if we are not rotating with trackball in this window
			if (!gTrackball || (gTrackingViewInfo != self)) {
				[_resetview updateObjectRotationForTimeDelta: deltaTime]; // update object rotation
			}
			shouldDraw = YES; // force redraw
		}
        printf("timer on %f %f %f\n", currentTime, time_anime, deltaTime);
	}
	time_anime = CFAbsoluteTimeGetCurrent (); //reset time in all cases
	// if we have current messages
	[self drawRectforError:[self bounds]]; // redraw now instead dirty to enable updates during live resize
     */
}

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
        message_opacity = 0;
        [self UpdateImage];
    }
    return;
}

// ---------------------------------
#pragma mark ---- IB Actions ----

- (void) setViewerType:(NSInteger) selected;
{
	if(selected == VIEW_3D
	   || selected == VIEW_STEREO){
		leftBottunFlag = ROTATE;
	}
	else{
		leftBottunFlag = PAN;
	};
}

-(void) setAnimate:(NSInteger)flag
{
	fAnimate = flag;
}

-(void) setInfo:(NSInteger)flag
{
//    [_cocoaGLMessages setDrawInfoFlag:flag];
	[self setNeedsDisplay: YES];
}

-(void) setQuickHelp:(NSInteger)flag
{
//	[_cocoaGLMessages  setQuickHelpFlag:flag];
	[self setNeedsDisplay: YES];
}

// ---------------------------------

- (void) Resetview
{
	kemoviewer_reset_to_init_angle();

	[self updateProjection];
	[self swapbuffer_cocoa];
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

	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	int anaglyphFlag = [[defaults stringForKey:@"AnaglyphFlag"] intValue];
	
	kemoview_set_view_integer(ISET_SHUTTER, SHUTTER_OFF);
	kemoview_set_view_integer(ISET_ANAGYLYPH, anaglyphFlag);
	
	[self prepareKemoOpenGL];
	
	kemoviewer_reset_to_init_angle();
	kemoview_init_lighting();

	// set start values...
	fAnimate =  0;
	
    [NSColor setIgnoresAlpha:NO];
	
	// start animation timer
/*
	time_anime = CFAbsoluteTimeGetCurrent ();  // set animation time start time
	timer_anime = [NSTimer timerWithTimeInterval:(1.0f/60.0f) target:self selector:@selector(animationTimer:) userInfo:nil repeats:NO];
	[[NSRunLoop currentRunLoop] addTimer:timer_anime forMode:NSDefaultRunLoopMode];
	[[NSRunLoop currentRunLoop] addTimer:timer_anime forMode:NSEventTrackingRunLoopMode]; // ensure timer fires during resize
  */  
    
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


-(void) setFrameSize:(NSSize)newSize
{
    [self resizeGL];
    return;
}
@end

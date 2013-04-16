//
//  KemoViewerOpenGLView.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/10/08.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoViewerOpenGLView.h"

// ==================================

GLint XpixelGLWindow, YpixelGLWindow;


// single set of interaction flags and states
GLdouble gDollyPanStartPoint[2] = {0.0, 0.0};
GLboolean gDolly = GL_FALSE;
GLboolean gPan = GL_FALSE;
GLboolean gTrackball = GL_FALSE;
KemoViewerOpenGLView * gTrackingViewInfo = NULL;

@implementation KemoViewerOpenGLView
// ---------------------------------

// get number of pixels for OpenGL Window
- (void) setRetinaMode
{
    NSRect rectView_DISP = [self bounds];
    if(XpixelGLWindow > rectView_DISP.size.width){
        set_kemoview_retinamode(IONE);
    } else {
        set_kemoview_retinamode(IZERO);
    };
    return;
}

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
        
        [self setRetinaMode];
//        printf("Pixel size %d, %d\n",XpixelGLWindow, YpixelGLWindow);
        
        iflag_updated = 1;
    }
    return iflag_updated;
}

// update the projection matrix based on camera and view info
- (void) updateProjection
{
    [[self openGLContext] makeCurrentContext];
	// set projection
	update_kemoviewer_distance();
}

// ---------------------------------

// updates the contexts model view matrix for object and camera moves
- (void) updateModelView
{
    [[self openGLContext] makeCurrentContext];
	
	// move view
	[self modify_view_Cocoa];
}

// ---------------------------------

// handles resizing of GL need context update and if the window dimensions change, a
// a window dimension update, reseting of viewport and an update of the projection matrix
- (void) resizeGL
{
	// ensure camera knows size changed
    int iflag_updated = [self getViewSize];
	if (iflag_updated != 0) {
		
		update_projection_by_kemoviewer_size(XpixelGLWindow, YpixelGLWindow);
        
        [[self openGLContext] makeCurrentContext];
		[_cocoaGLMessages updateInfoString:XpixelGLWindow:YpixelGLWindow];
		
		/* Reallocate memory for bitmapimage*/
		[bmpRep release];
		bmpRep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nil
														 pixelsWide: XpixelGLWindow
														 pixelsHigh: YpixelGLWindow
													  bitsPerSample: 8 
													samplesPerPixel: 3
														   hasAlpha: NO  
														   isPlanar: NO 
													 colorSpaceName:NSDeviceRGBColorSpace 
														//	bytesPerRow: (XpixelGLWindow*3) //pixelsWide*samplesPerPixel 
														// bitsPerPixel: (8*3)   //bitsPerSample*samplesPerPixel
														bytesPerRow: (XpixelGLWindow*3) //pixelsWide*samplesPerPixel 
													   bitsPerPixel: 0  //bitsPerSample*samplesPerPixel
				  ]; 
		
	}
}

-(void) modify_view_Cocoa
{	
	rotate_kemoview();
	reset_kemoviewer_animation();
	[_resetview UpdateParameters];
	return;
};


- (void) swapbuffer_cocoa{
	[[self openGLContext] flushBuffer];
	[self setNeedsDisplay:YES];
}

// ---------------------------------

// move camera in z axis
-(void)mouseDolly: (NSPoint) location
{
	kemoviewer_mousedolly(gDollyPanStartPoint, (GLdouble) location.x, (GLdouble) location.y);
}
	
// ---------------------------------
	
// move camera in x/y plane
- (void)mousePan: (NSPoint) location
{
	kemoviewer_mousepan(gDollyPanStartPoint, (GLdouble) location.x, (GLdouble) location.y);
}

// ---------------------------------


- (void) SaveGLBufferToFileNoStep:(NSInteger)ImageFormatId:(NSString *)fileHeader
{
	write_kemoviewer_window_to_file((int) ImageFormatId, [fileHeader UTF8String]);
	return;
}
- (void) SaveGLBufferToFileWithStep:(NSInteger) ImageFormatId:(NSInteger) istep:(NSString *)fileHeader
{
	write_kemoviewer_window_step_file((int) ImageFormatId, (int) istep, [fileHeader UTF8String]);
	return;
}

- (void) GetGLBufferForMovie:(NSImage *)imageBuffer{
	glReadBuffer(GL_FRONT);
	glPixelStorei(GL_PACK_ALIGNMENT, IONE);
	glReadPixels(IZERO, IZERO, XpixelGLWindow, YpixelGLWindow, GL_RGB,
				 GL_UNSIGNED_BYTE, [bmpRep bitmapData]);

	[imageBuffer addRepresentation:bmpRep];
	[imageBuffer setFlipped:YES];
	[imageBuffer lockFocusOnRepresentation:bmpRep]; // This will flip the rep.
	[imageBuffer unlockFocus];
	return;
}


-(id) DrawRotation:(NSNumber *)degree:(NSNumber *)rotationaxis
{
	NSInteger id_rotaxis = [rotationaxis intValue];
	NSInteger int_degree = [degree intValue];

    set_current_kemoview(id_window);
	set_kemoview_animation_rot_axis((int) id_rotaxis);
	set_kemoview_animation_rot_angle((int) int_degree);
	draw_kemoviewer_c();
	rotate_kemoview();
	
	[self swapbuffer_cocoa];
	return self;
}

-(id) DrawEvolution:(NSInteger)timeStep
{	
    set_current_kemoview(id_window);
	evolution_viewer((int) timeStep);
	draw_kemoviewer_c();
	modify_view_kemoview();
	
	[self swapbuffer_cocoa];
	return self;
}

-(void) UpdateImage
{
    set_current_kemoview(id_window);
	draw_kemoviewer_c();
	[self swapbuffer_cocoa];
	return;
}

- (void) drawRect:(NSRect)rect
{		
	// setup viewport and prespective
	[self resizeGL]; // forces projection matrix update (does test for size changes)
	[self updateModelView];  // update model view matrix for object
	
    if (fInfo) {
		[_cocoaGLMessages  drawInfo:XpixelGLWindow:YpixelGLWindow];
	}
	
	[self swapbuffer_cocoa];
}

// ---------------------------------

// per-window timer function, basic time based animation preformed here
- (void)animationTimer:(NSTimer *)timer
{
	BOOL shouldDraw = NO;
	if (fAnimate) {
		CFTimeInterval deltaTime = CFAbsoluteTimeGetCurrent () - time;
		if (deltaTime > 10.0) // skip pauses
			return;
		else {
			// if we are not rotating with trackball in this window
			if (!gTrackball || (gTrackingViewInfo != self)) {
				[_resetview updateObjectRotationForTimeDelta: deltaTime]; // update object rotation
			}
			shouldDraw = YES; // force redraw
		}
	}
	time = CFAbsoluteTimeGetCurrent (); //reset time in all cases
	// if we have current messages
	[self drawRect:[self bounds]]; // redraw now instead dirty to enable updates during live resize
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
		fAnimate = 0;
	};
	set_viewtype_glut(selected);
}

-(void) setAnimate:(NSInteger)flag
{
	fAnimate = flag;
}

-(void) setInfo:(NSInteger)flag
{
	fInfo = flag;
	[self setNeedsDisplay: YES];
}

-(void) setQuickHelp:(NSInteger)flag
{
	[_cocoaGLMessages  setQuickHelpFlag:flag];
	[self setNeedsDisplay: YES];
}

// ---------------------------------

- (void) Resetview
{
	reset_kemoviewer_to_init_angle();

	[self updateProjection];
	[self swapbuffer_cocoa];
}

// ---------------------------------

#pragma mark ---- Method Overrides ----

-(void)keyDown:(NSEvent *)theEvent
{
    NSString *characters = [theEvent characters];
    if ([characters length]) {
        unichar character = [characters characterAtIndex:0];
		switch (character) {
			case 'c':
				// toggle caps
				fDrawCaps = 1 - fDrawCaps;
				[self setNeedsDisplay: YES];
				break;
		}
	}
}

// ---------------------------------

- (void)mouseDown:(NSEvent *)theEvent // trackball
{
    if ([theEvent modifierFlags] & NSControlKeyMask) // send to pan
		[self rightMouseDown:theEvent];
	else if ([theEvent modifierFlags] & NSAlternateKeyMask) // send to dolly
		[self otherMouseDown:theEvent];
	else if(leftBottunFlag == PAN)
		[self rightMouseDown:theEvent];
	else {
		NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
		location.y = YpixelGLWindow - location.y;
		gDolly = GL_FALSE; // no dolly
		gPan = GL_FALSE; // no pan
		gTrackball = GL_TRUE;
		kemoview_startTrackball(location.x, -location.y);
		gTrackingViewInfo = self;
	}
}

// ---------------------------------

- (void)rightMouseDown:(NSEvent *)theEvent // pan
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
/*	
	if (gTrackball) { // if we are currently tracking, end trackball
		drugging_addToRotationTrackball();
	}
 */
	gDolly = GL_FALSE; // no dolly
	gPan = GL_TRUE; 
	gTrackball = GL_FALSE; // no trackball
	gDollyPanStartPoint[0] = (GLdouble) location.x;
	gDollyPanStartPoint[1] = (GLdouble) location.y;
	gTrackingViewInfo = self;
}

// ---------------------------------

- (void)otherMouseDown:(NSEvent *)theEvent //dolly
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
/*	
	if (gTrackball) { // if we are currently tracking, end trackball
		drugging_addToRotationTrackball();
	}
 */
	gDolly = GL_TRUE;
	gPan = GL_FALSE; // no pan
	gTrackball = GL_FALSE; // no trackball
	gDollyPanStartPoint[0] = (GLdouble) location.x;
	gDollyPanStartPoint[1] = (GLdouble) location.y;
	gTrackingViewInfo = self;
}

// ---------------------------------

- (void)mouseUp:(NSEvent *)theEvent
{
    set_current_kemoview(id_window);

	if (gDolly) { // end dolly
		gDolly = GL_FALSE;
	} else if (gPan) { // end pan
		gPan = GL_FALSE;
	} else if (gTrackball) { // end trackball
		gTrackball = GL_FALSE;
/*		drugging_addToRotationTrackball();*/
		[_resetview UpdateParameters];
	} 
	draw_kemoviewer_c();
	gTrackingViewInfo = NULL;
	[self setNeedsDisplay: YES];
}

// ---------------------------------

- (void)rightMouseUp:(NSEvent *)theEvent
{
	[self mouseUp:theEvent];
}

// ---------------------------------

- (void)otherMouseUp:(NSEvent *)theEvent
{
	[self mouseUp:theEvent];
}

// ---------------------------------

- (void)mouseDragged:(NSEvent *)theEvent
{
	NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
	location.y = YpixelGLWindow - location.y;
	if (gTrackball) {
		kemoview_rollToTrackball (location.x, -location.y);
		drugging_addToRotationTrackball();
		kemoview_startTrackball(location.x, -location.y);
		[self setNeedsDisplay: YES];
	} else if (gDolly) {
		[self mouseDolly: location];
		[self updateProjection];  // update projection matrix (not normally done on draw)
		[self setNeedsDisplay: YES];
	} else if (gPan) {
		[self mousePan: location];
		[self setNeedsDisplay: YES];
	}
}

// ---------------------------------

- (void)scrollWheel:(NSEvent *)theEvent
{
	float wheelDelta = [theEvent deltaX] +[theEvent deltaY] + [theEvent deltaZ];
	if (wheelDelta)
	{
		kemoviewer_zooming((GLdouble) wheelDelta);
		[self updateProjection];  // update projection matrix (not normally done on draw)
		[self setNeedsDisplay: YES];
	}
}

// ---------------------------------

- (void)rightMouseDragged:(NSEvent *)theEvent
{
	[self mouseDragged: theEvent];
}

// ---------------------------------

- (void)otherMouseDragged:(NSEvent *)theEvent
{
	[self mouseDragged: theEvent];
}

// ---------------------------------

- (void)magnifyWithEvent:(NSEvent *)theEvent
{
    GLdouble newScale = 200.0*[theEvent magnification];
    kemoviewer_zooming(newScale);
    [self updateProjection];
    [self setNeedsDisplay: YES];
}

// ---------------------------------

- (BOOL)acceptsFirstResponder{return YES;}
- (BOOL)becomeFirstResponder{return YES;}
- (BOOL)resignFirstResponder{return YES;}

// ---------------------------------

- (IBAction)SendToClipAsPDF:(id)sender
{
	NSImage *imageBuffer = [[NSImage alloc] init];
    NSData *poTiffData;
	NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];
	[pasteboard declareTypes:[NSArray arrayWithObjects:NSPasteboardTypeTIFF, nil] owner:nil];

	[self GetGLBufferForMovie:imageBuffer];
	poTiffData = [imageBuffer TIFFRepresentation];

	[pasteboard setData:poTiffData forType:NSTIFFPboardType];

	[pasteboard release];
	[poTiffData release];
	[imageBuffer release];
}

// ---------------------------------

// set initial OpenGL state (current context is set)
// called after context is created
- (void) prepareOpenGL
{
	GLfloat BgColor4f[4];

    int swapInt = 1;
	
    [[self openGLContext] setValues:&swapInt forParameter:NSOpenGLCPSwapInterval]; // set to vbl sync
	
	// init GL stuff here
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_LEQUAL);
	
	glShadeModel(GL_SMOOTH);    
	glEnable(GL_CULL_FACE);
	glFrontFace(GL_CCW);
	glPolygonOffset (1.0f, 1.0f);
	
	int iflag_updates = [self getViewSize];
    [self setRetinaMode];
    
    if(iflag_updates != 0) set_kemoview_windowsize(XpixelGLWindow, YpixelGLWindow);
		
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	set_kemoview_background_color(BgColor4f);

	glPushMatrix();
}
// ---------------------------------

- (void) awakeFromNib
{
    id_window = send_current_kemoview();
    set_current_kemoview(id_window);

    printf("awakeFromNib in KemoViewerOpenGLView on %d\n", id_window);
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	NSString *pickSurfName = [defaults stringForKey:@"PickSurfaceCommand"];
	int anaglyphFlag = [[defaults stringForKey:@"AnaglyphFlag"] intValue];
	
	if(!pickSurfName){
		printf("Empty default pick surface\n");
        set_to_pick_surface_command("pick_surface");
	} else {
        set_to_pick_surface_command([pickSurfName UTF8String]);
	};

	set_to_stereo_shutter(SHUTTER_OFF);
	set_to_iflag_anaglyph(anaglyphFlag);
	
	[self prepareOpenGL];
	
	reset_kemoviewer_to_init_angle();
	kemoviewer_initial_lighting();

	draw_kemoviewer_c();
	
	// set start values...
	fInfo =     0;
	fAnimate =  0;
	
    [NSColor setIgnoresAlpha:NO];
	// start animation timer
	time = CFAbsoluteTimeGetCurrent ();  // set animation time start time
	timer = [NSTimer timerWithTimeInterval:(1.0f/60.0f) target:self selector:@selector(animationTimer:) userInfo:nil repeats:YES];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];
	[[NSRunLoop currentRunLoop] addTimer:timer forMode:NSEventTrackingRunLoopMode]; // ensure timer fires during resize
 }
@end

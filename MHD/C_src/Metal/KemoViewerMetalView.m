/*
//  KemoViewerMetalView.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/1/23.
*/

#import "KemoViewerMetalView.h"

@implementation KemoViewerMetalView

-(void)InitBackGroundColor
{
    float BgColor4f[4];
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	BgColor4f[0] = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	BgColor4f[1] = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	BgColor4f[2] = [[defaults stringForKey:@"BackGroundBlue"] floatValue];
	BgColor4f[3] = 1.0;
	kemoview_set_background_color(BgColor4f);
    return;
};

-(void) updateBackground
{
    float bgcolor[4];
    [self InitBackGroundColor];
	kemoview_get_background_color(bgcolor);
    self.clearColor = MTLClearColorMake(bgcolor[0], bgcolor[1], bgcolor[2], bgcolor[3]);
    return;
}

- (void) setRetinaMode
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    int XpixelGLWindow = rectView.size.width;
    
    NSRect rectView_DISP = [self bounds];
    int XpixelRectView = rectView_DISP.size.width;
    
    if(XpixelGLWindow > XpixelRectView){
        kemoview_set_retinamode(IONE);
    } else {
        kemoview_set_retinamode(IZERO);
    };
    return;
}

- (void) setViewerSize
{
    [self setRetinaMode];
    NSRect rectView_DISP = [self bounds];
    [_kemoviewer setFrameSize:rectView_DISP.size];
}


-(void) UpdateImage
{
    [_kemoviewer UpdateImage];
    [self setNeedsDisplay: YES];
    return;
}
-(void) QuickUpdateImage
{
    [_kemoviewer QuickUpdateImage];
    [self setNeedsDisplay: YES];
    return;
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
    gDolly =     FALSE;
    gPan =       FALSE;
    gTrackball = FALSE;

    if ([theEvent modifierFlags] & NSEventModifierFlagControl) // send to pan
        [self rightMouseDown:theEvent];
    else if ([theEvent modifierFlags] & NSEventModifierFlagShift) // send to dolly
        [self otherMouseDown:theEvent];
    else if(leftBottunFlag == PAN)
        [self rightMouseDown:theEvent];
    else {
        NSRect rectView = [self convertRectToBacking:[self bounds]];
        CGFloat YpixelGLWindow = rectView.size.height;
        NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
        double xmove = (double) location.x;
        double ymove = (double) (location.y - YpixelGLWindow);
        gDolly =     FALSE; // no dolly
        gPan =       FALSE; // no pan
        gTrackball = TRUE;
        kemoview_startTrackball(xmove, ymove);
//        gTrackingViewInfo = self;
    }
    [self QuickUpdateImage];
    return;
}
// ---------------------------------
- (void)rightMouseDown:(NSEvent *)theEvent // pan
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (location.y - YpixelGLWindow);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball();
    }
 */
    gDolly =     FALSE; // no dolly
    gPan =       TRUE;
    gTrackball = FALSE; // no trackball
    gDollyPanStartPoint[0] =  xmove;
    gDollyPanStartPoint[1] = -ymove;
//    gTrackingViewInfo = self;
}

// ---------------------------------
- (void)otherMouseDown:(NSEvent *)theEvent //dolly
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (YpixelGLWindow - location.y);
/*
    if (gTrackball) { // if we are currently tracking, end trackball
        kemoview_drugging_addToRotationTrackball();
    }
 */
    gDolly =     TRUE;
    gPan =       FALSE; // no pan
    gTrackball = FALSE; // no trackball
    gDollyPanStartPoint[0] =  xmove;
    gDollyPanStartPoint[1] =  ymove;
//    gTrackingViewInfo = self;
}

// ---------------------------------
- (void)mouseUp:(NSEvent *)theEvent
{
    kemoview_set_single_viewer_id(id_window);

    if (gDolly) { // end dolly
        gDolly =     FALSE;
    } else if (gPan) { // end pan
        gPan =       FALSE;
    } else if (gTrackball) { // end trackball
        gTrackball = FALSE;
/*        kemoview_drugging_addToRotationTrackball();*/
        [_resetview UpdateParameters];
    }
//    gTrackingViewInfo = NULL;

    [self UpdateImage];
}

// ---------------------------------
- (void)mouseDragged:(NSEvent *)theEvent
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (YpixelGLWindow - location.y);
    if(gTrackball) {
        kemoview_rollToTrackball (xmove, -ymove);
        kemoview_drugging_addToRotationTrackball();
        kemoview_startTrackball(xmove, -ymove);
    } else if (gDolly) {
        kemoview_mousedolly(gDollyPanStartPoint, xmove, ymove);
    } else if (gPan) {
        kemoview_mousepan(gDollyPanStartPoint, xmove, ymove);
    }
    [self QuickUpdateImage];
    return;
}

// ---------------------------------
- (void)scrollWheel:(NSEvent *)theEvent
{
    float wheelDelta = [theEvent deltaX] + [theEvent deltaY] + [theEvent deltaZ];
    if (wheelDelta)
    {
        kemoview_zooming((double) wheelDelta);
    }
    [self QuickUpdateImage];
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
    double newScale = 200.0*[theEvent magnification];
    kemoview_zooming(newScale);
    [self QuickUpdateImage];
}

// ---------------------------------

- (BOOL)acceptsFirstResponder{return YES;}
- (BOOL)becomeFirstResponder{return YES;}
- (BOOL)resignFirstResponder{return YES;}

@end

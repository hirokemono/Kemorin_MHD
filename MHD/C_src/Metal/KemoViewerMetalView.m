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
    _gDolly =     FALSE; // no dolly
    _gPan =       TRUE;
    _gTrackball = FALSE; // no trackball
    _gDollyPanStartPoint[0] =  xmove;
    _gDollyPanStartPoint[1] = -ymove;
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
    _gDolly =     TRUE;
    _gPan =       FALSE; // no pan
    _gTrackball = FALSE; // no trackball
    _gDollyPanStartPoint[0] =  xmove;
    _gDollyPanStartPoint[1] =  ymove;
//    gTrackingViewInfo = self;
}
// ---------------------------------
- (void)mouseDown:(NSEvent *)theEvent // trackball
{
    _gDolly =     FALSE;
    _gPan =       FALSE;
    _gTrackball = FALSE;

    if ([theEvent modifierFlags] & NSEventModifierFlagControl) // send to pan
        [self rightMouseDown:theEvent];
    else if ([theEvent modifierFlags] & NSEventModifierFlagShift) // send to dolly
        [self otherMouseDown:theEvent];
    else if(_leftBottunFlag == PAN)
        [self rightMouseDown:theEvent];
    else {
        NSRect rectView = [self convertRectToBacking:[self bounds]];
        CGFloat YpixelGLWindow = rectView.size.height;
        NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
        double xmove = (double) location.x;
        double ymove = (double) (location.y - YpixelGLWindow);
        _gDolly =     FALSE; // no dolly
        _gPan =       FALSE; // no pan
        _gTrackball = TRUE;
        kemoview_startTrackball(xmove, ymove);
//        gTrackingViewInfo = self;
    }
    [self QuickUpdateImage];
    return;
}

- (void)mouseDragged:(NSEvent *)theEvent
{
    NSRect rectView = [self convertRectToBacking:[self bounds]];
    CGFloat YpixelGLWindow = rectView.size.height;
    NSPoint location = [self convertPoint:[theEvent locationInWindow] fromView:nil];
    double xmove = (double) location.x;
    double ymove = (double) (YpixelGLWindow - location.y);
    if(_gTrackball) {
        kemoview_rollToTrackball (xmove, -ymove);
        kemoview_drugging_addToRotationTrackball();
        kemoview_startTrackball(xmove, -ymove);
    } else if (_gDolly) {
        kemoview_mousedolly(_gDollyPanStartPoint, xmove, ymove);
    } else if (_gPan) {
        kemoview_mousepan(_gDollyPanStartPoint, xmove, ymove);
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

@end

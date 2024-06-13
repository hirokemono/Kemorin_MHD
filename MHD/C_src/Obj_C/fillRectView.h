//
//  fillRectView.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 11/08/17.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//


@import Cocoa;

#import "KemoviewerController.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"

#define RECTCOUNT   128

@interface fillRectView : NSView
{
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet KemoViewerObject *_kmv;
    
    NSRect	boxRect;
    NSRect  rectList1[RECTCOUNT];
    NSRect  rectList2[RECTCOUNT];
    NSColor *colors[RECTCOUNT];
	NSColor *color_w_opaciy[RECTCOUNT];
}

- (void)UpdateColorbar;
- (void)SetColorRectangles:(struct kemoviewer_type *) kemo_sgl;
- (void)DrawColorMarks;
- (void)drawString:(NSString*)string x:(double)x y:(double)y;

@end

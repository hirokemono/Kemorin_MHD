

@import Cocoa;

#import "KemoViewerObject.h"
#include "Kemoviewer.h"

#define RECTCOUNT   128

@interface fillRectView : NSView
{
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

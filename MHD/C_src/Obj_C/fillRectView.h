

@import Cocoa;

#import "KemoViewerObject.h"
#include "Kemoviewer.h"

@interface fillRectView : NSView
{
    IBOutlet KemoViewerObject *_kmv;
}
- (void)UpdateColorbar;
- (void)DrawColorBarFlame:(NSRect)frameRect;
- (void)DrawColorMarks:(NSRect)frameRect
              kemoview:(struct kemoviewer_type *) kemo_sgl;
- (void)drawString:(NSString*)string x:(double)x y:(double)y;

@end

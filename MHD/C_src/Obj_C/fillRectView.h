#import <Cocoa/Cocoa.h>

#include "kemoviewer.h"

@interface fillRectView : NSView
{
}
- (void)UpdateColorbar;
- (void)DrawColorBarFlame:(NSRect)frameRect;
- (void)DrawColorMarks:(NSRect)frameRect;
- (void)drawString:(NSString*)string x:(double)x y:(double)y;

@end

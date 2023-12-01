//
//  KemoViewerMetalViewController.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/2/23.
//

#ifndef KemoViewerMetalViewController_h
#define KemoViewerMetalViewController_h

@import Cocoa;
@import AppKit;
@import MetalKit;

#import "AAPLRenderer.h"
#import "KemoViewerMetalView.h"
#include "kemoviewer.h"

@interface KemoViewerMetalViewController : NSViewController{
}

-(void) awakeFromNib;
-(void) viewDidLoad;

- (void) RenderUpdate;
- (void) viewDidLayout;

-(unsigned char *) getRenderedbyMetalToBGRA:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte;
-(CGImageRef) getRenderedbyMetalToCGref;
-(void) getRenderedbyMetal:(NSBitmapImageRep *) imageRep;

@end


#endif /* KemoViewerMetalViewController_h */

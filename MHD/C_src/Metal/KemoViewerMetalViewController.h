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
#import "KemoViewerObject.h"

#include "Kemoviewer.h"

@interface KemoViewerMetalViewController : NSViewController{
    IBOutlet KemoViewerObject *_kmv;
}

-(void) awakeFromNib;
-(void) viewDidLoad;

- (void) RenderUpdate;
- (void) viewDidLayout;

-(unsigned char *) getRenderedbyMetalToBGRA:(NSUInteger *) pix_xy
                               PixelPerByte:(NSUInteger *) pixelByte
                                   kemoview:(struct kemoviewer_type *) kemo_sgl;
-(CGImageRef) getRenderedbyMetalToCGref:(struct kemoviewer_type *) kemo_sgl;
-(void) getRenderedbyMetal:(NSBitmapImageRep *) imageRep
                  kemoview:(struct kemoviewer_type *) kemo_sgl;

@end


#endif /* KemoViewerMetalViewController_h */

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

-(void) viewDidLayout;
-(nonnull void *) loadImageOutputTexture;

-(void) FastUpdateImageController:(NSImage *) Image;

@end


#endif /* KemoViewerMetalViewController_h */

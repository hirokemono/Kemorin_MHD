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

@interface KemoViewerMetalViewController : NSViewController{
}

-(void) awakeFromNib;
-(void) viewDidLoad;
@end


#endif /* KemoViewerMetalViewController_h */

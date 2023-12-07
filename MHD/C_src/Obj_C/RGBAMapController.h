//
//  RGBAMapController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "ColorMapController.h"
#import "OpacityMapController.h"
#import "fillRectView.h"
#import "KemoViewerObject.h"

#include "Kemoviewer.h"


@interface RGBAMapController : NSObject {

    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerObject *_kmv;

	IBOutlet ColorMapController*   _colorMapObject;
	IBOutlet OpacityMapController* _opacityMapObject;

	CGFloat DataMinimum;
	CGFloat DataMaximum;
}
@property CGFloat DataMinimum;
@property CGFloat DataMaximum;


- (void)awakeFromNib;
- (void)updateColormapParameter:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)SaveColormapFile:(id)pId;
- (IBAction)LoadColormapFile:(id)pId;

@end

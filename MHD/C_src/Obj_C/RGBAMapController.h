//
//  RGBAMapController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "KemoviewerController.h"
#import "ColorMapController.h"
#import "OpacityMapController.h"
#import "fillRectView.h"
#import "KemoViewerObject.h"

#include "Kemoviewer.h"


@interface RGBAMapController : NSObject {

    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet fillRectView* _fillRectView;
    IBOutlet KemoViewerObject *_kmv;

	IBOutlet ColorMapController*   _colorMapObject;
	IBOutlet OpacityMapController* _opacityMapObject;
}

- (void) UpdateColormapView:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)SaveColormapFile:(id)pId;
- (IBAction)LoadColormapFile:(id)pId;

@end

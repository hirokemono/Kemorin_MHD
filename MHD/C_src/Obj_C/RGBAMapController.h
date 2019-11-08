//
//  RGBAMapController.h
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "ColorMapController.h"
#import "OpacityMapController.h"
#import "fillRectView.h"
#include "kemoviewer.h"


@interface RGBAMapController : NSObject {

    IBOutlet NSWindow*  window;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	
	IBOutlet ColorMapController*   _colorMapObject;
	IBOutlet OpacityMapController* _opacityMapObject;

	CGFloat DataMinimum;
	CGFloat DataMaximum;
}
@property CGFloat DataMinimum;
@property CGFloat DataMaximum;


- (void)awakeFromNib;
- (void)updateColormapParameter;

- (IBAction)SaveColormapFile:(id)pId;
- (IBAction)LoadColormapFile:(id)pId;

@end

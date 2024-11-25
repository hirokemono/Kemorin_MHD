//
//  OpacityMapController.h
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "fillRectView.h"
#import "KemoviewerController.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface OpacityMapController : NSObject {
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoviewerController*  _kemoviewControl;
    IBOutlet KemoViewerObject *_kmv;

	NSInteger  NumOpacityTable;
	NSMutableArray *OpacityTableField;
	NSMutableArray *OpacityTableOpacity;
	IBOutlet NSTableView * idOpacityTableView;
	
	IBOutlet id _OpacityTableView;
    IBOutlet fillRectView* _fillRectView;
}
@property (assign) NSMutableArray * OpacityTableField;
@property (assign) NSMutableArray * OpacityTableOpacity;
@property (assign) NSTableView * idOpacityTableView;

- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void) SetOpacityTables:(struct kemoviewer_type *) kemo_sgl;
- (IBAction) UpdateOpacityTables:(id)pID;

@end

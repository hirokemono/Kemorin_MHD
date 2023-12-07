//
//  OpacityMapController.h
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "fillRectView.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface OpacityMapController : NSObject {
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerObject *_kmv;

	NSInteger  NumOpacityTable;
	NSMutableArray *OpacityTableField;
	NSMutableArray *OpacityTableOpacity;
	IBOutlet NSTableView * idOpacityTableView;
	
	IBOutlet id _OpacityTableView;
    IBOutlet fillRectView* _fillRectView;
	
	CGFloat DataMaximum;
	CGFloat DataMinimum;
}
@property (assign) NSMutableArray * OpacityTableField;
@property (assign) NSMutableArray * OpacityTableOpacity;
@property (assign) NSTableView * idOpacityTableView;
@property CGFloat DataMaximum;
@property CGFloat DataMinimum;


- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void)InitOpacityTables:(struct kemoviewer_type *) kemo_sgl;
- (void) SetOpacityTables:(struct kemoviewer_type *) kemo_sgl;
- (IBAction) UpdateOpacityTables:(id)pID;

@end

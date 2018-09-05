//
//  OpacityMapController.h
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "fillRectView.h"


@interface OpacityMapController : NSObject {
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;

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

- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void)InitOpacityTables;
- (void)SetOpacityTables;
- (IBAction) UpdateOpacityTables:(id)pID;

@end

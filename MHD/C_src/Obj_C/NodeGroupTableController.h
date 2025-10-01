//
//  NodeGroupTableController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/29.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface NodeGroupTableController : NSObject {
	
    IBOutlet KemoViewerMetalView * _metalView;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet id _nodeTableView;
	
	NSInteger  NumNodeGroup;
	NSMutableArray *NodeGroupDisplayNames;
	NSMutableArray *NodeGroupDisplayNodeFlags;
	
	NSString *selectedNodeGroupObjectType;

	IBOutlet id _NodeGrpNodeColorItem;	
	IBOutlet NSColorWell *nodeGrpNodeColorWell;
    NSColor *nsNodeGrpNodeColor;
	
}

- (id) init;
- (id) dealloc;

- (IBAction) ShowAllNodeGroupAction:(id)pId;
- (IBAction) HideAllNodeGroupAction:(id)pId;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView;

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (void)tableView:(NSTableView *)atableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;
- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn;

- (void)UpdateNodeTable:(struct kemoviewer_type *) kemo_sgl;
- (IBAction)ChooseNodeGrpNodeColorAction:(id)sender;
- (IBAction)SetNodeGrpNodeColorAction:(id)sender;

@end

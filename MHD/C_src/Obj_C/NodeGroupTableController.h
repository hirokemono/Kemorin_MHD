//
//  NodeGroupTableController.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/29.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"


@interface NodeGroupTableController : NSObject {
	
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
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

- (int)numberOfRowsInTableView:(NSTableView *)aTableView;

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex;

- (void)tableView:(NSTableView *)atableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;
- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn;

- (void)UpdateNodeTable;
- (IBAction)ChooseNodeGrpNodeColorAction:(id)sender;
- (IBAction)SetNodeGrpNodeColorAction:(id)sender;

@end

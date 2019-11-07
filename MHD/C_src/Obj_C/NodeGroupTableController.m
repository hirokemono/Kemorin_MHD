//
//  NodeGroupTableController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/29.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "NodeGroupTableController.h"
#include "kemoviewer.h"


@implementation NodeGroupTableController
- (id) init
{
	NumNodeGroup = 0;
	NodeGroupDisplayNames= [[NSMutableArray alloc] init];
	NodeGroupDisplayNodeFlags=  [[NSMutableArray alloc] init];
	
	return self;
}

- (id) dealloc
{
	[NodeGroupDisplayNames release];
	[NodeGroupDisplayNodeFlags  release];
	
	[super dealloc];
	return self;
}

- (IBAction) ShowAllNodeGroupAction:(id)pId{
	int i;
	
	NSLog(@"selectedNodeGroupObjectType %@", selectedNodeGroupObjectType);
	if([selectedNodeGroupObjectType isEqualToString:@"NodGrpNode"]) {
		NSLog(@"Set all NodGrpNode");
		[NodeGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumNodeGroup;i++){
			[NodeGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			NSLog(@"Set all nodes %d", i);
			kemoview_set_draw_mesh_item(NODE_GRP_FLAG, SURFSOLID_TOGGLE, i, IONE);
		}
	}

    [self UpdateNodeTable];
	[_kemoviewer UpdateImage];
}

- (IBAction) HideAllNodeGroupAction:(id)pId
{
	int i;
	
	if([selectedNodeGroupObjectType isEqualToString:@"NodGrpNode"]) {
		[NodeGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumNodeGroup;i++){
			[NodeGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(NODE_GRP_FLAG, SURFSOLID_TOGGLE, i, IZERO);
		}
	}	

    [self UpdateNodeTable];
	[_kemoviewer UpdateImage];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return NumNodeGroup;
}

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex
{
	if([[aTableColumn identifier] isEqualToString:@"NodGrpNode"]) {
        return [NodeGroupDisplayNodeFlags objectAtIndex:rowIndex];
    }
    else if([[aTableColumn identifier] isEqualToString:@"NodGrpName"]) {
		return [NodeGroupDisplayNames objectAtIndex:rowIndex];
	}
    return nil;
}

- (void)tableView:(NSTableView *)aTableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;
{
    id	identifier;
	
    identifier = [tableColumn identifier];
    if([identifier isEqualToString:@"NodGrpNode"]) {
		kemoview_set_draw_mesh_item(NODE_GRP_FLAG, SURFSOLID_TOGGLE, rowIndex, [object intValue]);
		[NodeGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
	}

	[_kemoviewer UpdateImage];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedNodeGroupObjectType = [tableColumn identifier];
	return;
}

- (void) UpdateNodeTable
{
	int i, iflag;
    struct kv_string *groupname;
	NSString *stname;
	
//	printf("Update Node group map\n");
	[NodeGroupDisplayNames removeAllObjects];
	[NodeGroupDisplayNodeFlags removeAllObjects];
	NumNodeGroup = kemoview_get_num_of_mesh_group(NODE_GRP_FLAG);
	for(i=0;i<NumNodeGroup;i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_node_grp_name(groupname,i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);
    
		[NodeGroupDisplayNames      addObject:stname];
		iflag = kemoview_get_draw_mesh_item(NODE_GRP_FLAG, SURFSOLID_TOGGLE, i);
		[NodeGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_nodeTableView reloadData];
	
}

- (IBAction)ChooseNodeGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_NodeGrpNodeColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(NODE_GRP_FLAG, SURFSOLID_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}
- (IBAction)SetNodeGrpNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsNodeGrpNodeColor = [nodeGrpNodeColorWell color];
	[nsNodeGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(NODE_GRP_FLAG, SURFSOLID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
@end

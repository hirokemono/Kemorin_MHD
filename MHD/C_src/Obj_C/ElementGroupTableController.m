//
//  ElementGroupTableController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/28.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "ElementGroupTableController.h"
#include "kemoviewer.h"


@implementation ElementGroupTableController
- (id) init
{
	NumElementGroup = 0;
	ElementGroupDisplayNames= [[NSMutableArray alloc] init];
	ElementGroupDisplayPatchFlags= [[NSMutableArray alloc] init];
	ElementGroupDisplayWireFlags=  [[NSMutableArray alloc] init];
	ElementGroupDisplayNodeFlags=  [[NSMutableArray alloc] init];

	return self;
}

- (id) dealloc
{
	[ElementGroupDisplayNames release];
	[ElementGroupDisplayPatchFlags release];
	[ElementGroupDisplayWireFlags  release];
	[ElementGroupDisplayNodeFlags  release];
	[super dealloc];
	return self;
}

- (void) SetElementGroupLabels;
{
	int i;
    struct kv_string *groupname;
	NSString *stname;
	
	[ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = kemoview_get_num_of_mesh_group(ELEM_GRP_FLAG);
	for(i=0;i<NumElementGroup;i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_ele_grp_name(groupname,i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);

        [ElementGroupDisplayNames      addObject:stname];
		[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
		[ElementGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[ElementGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[stname release];
	}
	[_elementTableView reloadData];
}

- (IBAction) ShowAllElementGroupAction:(id)pId{
	int i;
	
	if([selectedElementGroupObjectType isEqualToString:@"EleGrpPatch"]) {
		[ElementGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i, IONE);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedElementGroupObjectType UTF8String]]);
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i, IONE);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i, IONE);
		}
	}
    
    [self UpdateElementTable];
	[_kemoviewer UpdateImage];
}

- (IBAction) HideAllElementGroupAction:(id)pId
{
	int i;
	
	if([selectedElementGroupObjectType isEqualToString:@"EleGrpPatch"]) {
		[ElementGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i, IZERO);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i, IZERO);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i, IZERO);
		}
	}	
    [self UpdateElementTable];
	[_kemoviewer UpdateImage];
}


- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return NumElementGroup;
}

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex
{
	// 'on' の列
	if([[aTableColumn identifier] isEqualToString:@"EleGrpPatch"]) {
        return [ElementGroupDisplayPatchFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"EleGrpGrid"]) {
        return [ElementGroupDisplayWireFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"EleGrpNode"]) {
        return [ElementGroupDisplayNodeFlags objectAtIndex:rowIndex];
    }
	// 'name' の列
    else if([[aTableColumn identifier] isEqualToString:@"EleGrpName"]) {
		return [ElementGroupDisplayNames objectAtIndex:rowIndex];
	}
    
    // ここには来ないはず
    return nil;
}

- (void)tableView:(NSTableView *)aTableView
   setObjectValue:(id)object 
   forTableColumn:(NSTableColumn *)tableColumn 
			  row:(int)rowIndex;
{
    id	identifier;

    identifier = [tableColumn identifier];
    if([identifier isEqualToString:@"EleGrpPatch"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, rowIndex, [object intValue]);
		[ElementGroupDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
    }
    if([identifier isEqualToString:@"EleGrpGrid"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, rowIndex, [object intValue]);
		[ElementGroupDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
    if([identifier isEqualToString:@"EleGrpNode"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, rowIndex, [object intValue]);
		[ElementGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
	[_kemoviewer UpdateImage];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedElementGroupObjectType = [tableColumn identifier];
	return;
}

- (void) UpdateElementTable
{
	int i, iflag;
    struct kv_string *groupname;
	NSString *stname;
	
	// printf("Update Element group map\n");
	[ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = kemoview_get_num_of_mesh_group(ELEM_GRP_FLAG);
	for(i=0;i<NumElementGroup;i++){
        groupname = kemoview_alloc_kvstring();
		kemoview_get_ele_grp_name(groupname,i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);

        [ElementGroupDisplayNames      addObject:stname];
		iflag = kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i);
		[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i);
		[ElementGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i);
		[ElementGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_elementTableView reloadData];

}

- (IBAction)ChooseEleGrpPatchColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpPatchColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseEleGrpLineColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpLineColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFGRID_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseEleGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpNodeColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFNOD_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)SetEleGrpPatchColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsEleGrpPatchColor = [eleGrpPatchColorWell color];
	[nsEleGrpPatchColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetEleGrpLineColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsEleGrpGridColor = [eleGrpGridColorWell color];
	[nsEleGrpGridColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFGRID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetEleGrpNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsEleGrpNodeColor = [eleGrpNodeColorWell color];
	[nsEleGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFNOD_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
@end

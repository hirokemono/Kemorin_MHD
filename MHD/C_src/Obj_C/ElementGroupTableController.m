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
	char name[4096];
	NSString *stname;
	
	[ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = send_ngrp_ele_sf();
	for(i=0;i<NumElementGroup;i++){
		send_ele_gp_name_sf(name,i);
		stname = [[NSString alloc] initWithUTF8String:name];
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
			set_to_draw_elegrp_solid(IONE,i);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedElementGroupObjectType UTF8String]]);
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			set_to_draw_elegrp_grid(IONE,i);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			set_to_draw_elegrp_nod(IONE,i);
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
			set_to_draw_elegrp_solid(IZERO,i);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			set_to_draw_elegrp_grid(IZERO,i);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			set_to_draw_elegrp_nod(IZERO,i);
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
		set_to_draw_elegrp_solid([object intValue],rowIndex);
		[ElementGroupDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
    }
    if([identifier isEqualToString:@"EleGrpGrid"]) {
		set_to_draw_elegrp_grid([object intValue],rowIndex);
		[ElementGroupDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
    if([identifier isEqualToString:@"EleGrpNode"]) {
		set_to_draw_elegrp_nod([object intValue],rowIndex);
		[ElementGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
	[_kemoviewer UpdateImage];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedElementGroupObjectType = [tableColumn identifier];
	return;
}

- (IBAction) UpdateElementTable
{
	int i, iflag;
	char name[4096];
	NSString *stname;
	
	// printf("Update Element group map\n");
	[ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = send_ngrp_ele_sf();
	for(i=0;i<NumElementGroup;i++){
		send_ele_gp_name_sf(name,i);
		stname = [[NSString alloc] initWithUTF8String:name];
		[ElementGroupDisplayNames      addObject:stname];
		iflag = send_draw_elegrp_solid(i);
		[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = send_draw_elegrp_grid(i);
		[ElementGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = send_draw_elegrp_nod(i);
		[ElementGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_elementTableView reloadData];

}

- (IBAction)ChooseEleGrpPatchColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpPatchColorItem selectedCell] tag];
	set_ele_grp_color_flag(SURFSOLID_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseEleGrpLineColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpLineColorItem selectedCell] tag];
	set_ele_grp_color_flag(SURFGRID_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseEleGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpNodeColorItem selectedCell] tag];
	set_ele_grp_color_flag(SURFNOD_TOGGLE, tag);

	[_kemoviewer UpdateImage];
}

- (IBAction)SetEleGrpPatchColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsEleGrpPatchColor = [eleGrpPatchColorWell color];
	[nsEleGrpPatchColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_ele_grp_color_code(SURFSOLID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetEleGrpLineColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsEleGrpGridColor = [eleGrpGridColorWell color];
	[nsEleGrpGridColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_ele_grp_color_code(SURFGRID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetEleGrpNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsEleGrpNodeColor = [eleGrpNodeColorWell color];
	[nsEleGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_ele_grp_color_code(SURFNOD_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
@end

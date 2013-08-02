//
//  SurfaceGroupTableController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/29.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "SurfaceGroupTableController.h"
#include "kemoviewer.h"


@implementation SurfaceGroupTableController
- (id) init
{
	NumSurfaceGroup = 0;
	SurfaceGroupDisplayNames= [[NSMutableArray alloc] init];
	SurfaceGroupDisplayPatchFlags= [[NSMutableArray alloc] init];
	SurfaceGroupDisplayWireFlags=  [[NSMutableArray alloc] init];
	SurfaceGroupDisplayNodeFlags=  [[NSMutableArray alloc] init];
	
	return self;
}

- (id) dealloc
{
	[SurfaceGroupDisplayNames release];
	[SurfaceGroupDisplayPatchFlags release];
	[SurfaceGroupDisplayWireFlags  release];
	[SurfaceGroupDisplayNodeFlags  release];
	[super dealloc];
	return self;
}

- (void) SetSurfaceGroupLabels:(id)pId
{
	int i;
	char name[4096];
	NSString *stname;
	
	[SurfaceGroupDisplayNames removeAllObjects];
	[SurfaceGroupDisplayPatchFlags removeAllObjects];
	[SurfaceGroupDisplayWireFlags removeAllObjects];
	[SurfaceGroupDisplayNodeFlags removeAllObjects];
	NumSurfaceGroup = send_ngrp_surf_sf();
	for(i=0;i<NumSurfaceGroup;i++){
		send_surf_gp_name_sf(name,i);
		stname = [[NSString alloc] initWithUTF8String:name];
		[SurfaceGroupDisplayNames      addObject:stname];
		[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
		[SurfaceGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[SurfaceGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[stname release];
	}
	[_surfaceTableView reloadData];
}

- (IBAction) ShowAllSurfaceGroupAction:(id)pId{
	int i;
	
	if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpPatch"]) {
		[SurfaceGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			set_to_draw_surfgrp_solid(IONE,i);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpGrid"]) {
		//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedSurfaceGroupObjectType UTF8String]]);
		[SurfaceGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			set_to_draw_surfgrp_grid(IONE,i);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpNode"]) {
		[SurfaceGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			set_to_draw_surfgrp_nod(IONE,i);
		}
	}
    [self UpdateSurfaceTable];
	[_kemoviewer UpdateImage];
}

- (IBAction) HideAllSurfaceGroupAction:(id)pId
{
	int i;
	
	if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpPatch"]) {
		[SurfaceGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			set_to_draw_surfgrp_solid(IZERO,i);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpGrid"]) {
		[SurfaceGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			set_to_draw_surfgrp_grid(IZERO,i);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpNode"]) {
		[SurfaceGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			set_to_draw_surfgrp_nod(IZERO,i);
		}
	}	
    [self UpdateSurfaceTable];
	[_kemoviewer UpdateImage];
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return NumSurfaceGroup;
}

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex
{
	// 'on' の列
	if([[aTableColumn identifier] isEqualToString:@"SurfGrpPatch"]) {
        return [SurfaceGroupDisplayPatchFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"SurfGrpGrid"]) {
        return [SurfaceGroupDisplayWireFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"SurfGrpNode"]) {
        return [SurfaceGroupDisplayNodeFlags objectAtIndex:rowIndex];
    }
	// 'name' の列
    else if([[aTableColumn identifier] isEqualToString:@"SurfGrpName"]) {
		return [SurfaceGroupDisplayNames objectAtIndex:rowIndex];
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
    if([identifier isEqualToString:@"SurfGrpPatch"]) {
		set_to_draw_surfgrp_solid([object intValue],rowIndex);
		[SurfaceGroupDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
    }
    if([identifier isEqualToString:@"SurfGrpGrid"]) {
		set_to_draw_surfgrp_grid([object intValue],rowIndex);
		[SurfaceGroupDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
	}
    if([identifier isEqualToString:@"SurfGrpNode"]) {
		set_to_draw_surfgrp_nod([object intValue],rowIndex);
		[SurfaceGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
	}
	[_kemoviewer UpdateImage];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedSurfaceGroupObjectType = [tableColumn identifier];
	return;
}

- (void) UpdateSurfaceTable
{
	int i, iflag;
	char name[4096];
	NSString *stname;
	
	[SurfaceGroupDisplayNames removeAllObjects];
	[SurfaceGroupDisplayPatchFlags removeAllObjects];
	[SurfaceGroupDisplayWireFlags removeAllObjects];
	[SurfaceGroupDisplayNodeFlags removeAllObjects];
	NumSurfaceGroup = send_ngrp_surf_sf();
	for(i=0;i<NumSurfaceGroup;i++){
		send_surf_gp_name_sf(name,i);
		stname = [[NSString alloc] initWithUTF8String:name];
		[SurfaceGroupDisplayNames      addObject:stname];
		iflag = send_draw_surfgrp_solid(i);
		[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = send_draw_surfgrp_grid(i);
		[SurfaceGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = send_draw_surfgrp_nod(i);
		[SurfaceGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_surfaceTableView reloadData];	
}


- (IBAction)ChooseSurfGrpPatchColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpPatchColorItem selectedCell] tag];
	set_surf_grp_color_flag(SURFSOLID_TOGGLE, tag);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseSurfGrpLineColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpLineColorItem selectedCell] tag];
	set_surf_grp_color_flag(SURFGRID_TOGGLE, tag);
	[_kemoviewer UpdateImage];
}

- (IBAction)ChooseSurfGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpNodeColorItem selectedCell] tag];
	set_surf_grp_color_flag(SURFNOD_TOGGLE, tag);
	[_kemoviewer UpdateImage];
}


- (IBAction)SetSurfGrpPatchColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsSurfGrpNodeColor = [surfGrpPatchColorWell color];
	[nsSurfGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_surf_grp_color_code(SURFSOLID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetSurfGrpLineColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsSurfGrpGridColor = [surfGrpGridColorWell color];
	[nsSurfGrpGridColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_surf_grp_color_code(SURFGRID_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
- (IBAction)SetSurfGrpNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	GLfloat colorcode4[4];
	nsSurfGrpNodeColor = [surfGrpNodeColorWell color];
	[nsSurfGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (GLfloat) redBG;
	colorcode4[1] =  (GLfloat) greenBG;
	colorcode4[2] =  (GLfloat) blueBG;
	colorcode4[3] =  (GLfloat) opacityBG;
	set_surf_grp_color_code(SURFNOD_TOGGLE, colorcode4);
	
	[_kemoviewer UpdateImage];
}
@end

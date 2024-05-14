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
@synthesize surfGrpAlpha;
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
    struct kv_string *groupname;
	NSString *stname;
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	[SurfaceGroupDisplayNames removeAllObjects];
	[SurfaceGroupDisplayPatchFlags removeAllObjects];
	[SurfaceGroupDisplayWireFlags removeAllObjects];
	[SurfaceGroupDisplayNodeFlags removeAllObjects];
	NumSurfaceGroup = kemoview_get_num_of_mesh_group(kemo_sgl, SURF_GRP_FLAG);
	for(i=0;i<NumSurfaceGroup;i++){
        groupname = kemoview_get_group_name(kemo_sgl, SURF_GRP_FLAG, i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);

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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpPatch"]) {
		[SurfaceGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFSOLID_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpGrid"]) {
		//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedSurfaceGroupObjectType UTF8String]]);
		[SurfaceGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFGRID_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpNode"]) {
		[SurfaceGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFNOD_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
    [self UpdateSurfaceTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction) HideAllSurfaceGroupAction:(id)pId
{
	int i;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpPatch"]) {
		[SurfaceGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFSOLID_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpGrid"]) {
		[SurfaceGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFGRID_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}
	else if([selectedSurfaceGroupObjectType isEqualToString:@"SurfGrpNode"]) {
		[SurfaceGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSurfaceGroup;i++){
			[SurfaceGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFNOD_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}	
    [self UpdateSurfaceTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}

- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return (int) NumSurfaceGroup;
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    id	identifier = [tableColumn identifier];
    if([identifier isEqualToString:@"SurfGrpPatch"]) {
		kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFSOLID_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[SurfaceGroupDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
    }
    if([identifier isEqualToString:@"SurfGrpGrid"]) {
		kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFGRID_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[SurfaceGroupDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
	}
    if([identifier isEqualToString:@"SurfGrpNode"]) {
		kemoview_set_draw_mesh_item(SURF_GRP_FLAG, SURFNOD_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[SurfaceGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
	}
	[_metalView UpdateImage:kemo_sgl];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedSurfaceGroupObjectType = [tableColumn identifier];
	return;
}

- (void) UpdateSurfaceTable:(struct kemoviewer_type *) kemo_sgl
{
	int i, iflag;
    struct kv_string *groupname;
	NSString *stname;
    float colorcode4[4];
    kemoview_get_mesh_color_code(kemo_sgl, SURF_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4);
    self.surfGrpAlpha = colorcode4[3];

	[SurfaceGroupDisplayNames removeAllObjects];
	[SurfaceGroupDisplayPatchFlags removeAllObjects];
	[SurfaceGroupDisplayWireFlags removeAllObjects];
	[SurfaceGroupDisplayNodeFlags removeAllObjects];
	NumSurfaceGroup = kemoview_get_num_of_mesh_group(kemo_sgl, SURF_GRP_FLAG);
	for(i=0;i<NumSurfaceGroup;i++){
        groupname = kemoview_get_group_name(kemo_sgl, SURF_GRP_FLAG, i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);

        [SurfaceGroupDisplayNames      addObject:stname];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, SURF_GRP_FLAG,
                                            SURFSOLID_TOGGLE, i);
		[SurfaceGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, SURF_GRP_FLAG,
                                            SURFGRID_TOGGLE, i);
		[SurfaceGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, SURF_GRP_FLAG,
                                            SURFNOD_TOGGLE, i);
		[SurfaceGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_surfaceTableView reloadData];	
}


- (IBAction)ChooseSurfGrpPatchColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpPatchColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_flag(SURF_GRP_FLAG, SURFSOLID_TOGGLE,
                                 (int) tag, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseSurfGrpLineColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpLineColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_flag(SURF_GRP_FLAG, SURFGRID_TOGGLE,
                                 (int) tag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseSurfGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_SurfGrpNodeColorItem selectedCell] tag];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_flag(SURF_GRP_FLAG, SURFNOD_TOGGLE,
                                 (int) tag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}


- (IBAction)SetSurfGrpPatchAlphaAction:(id)sender
{
    float colorcode4[4];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_get_mesh_color_code(kemo_sgl, SURF_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4);
    colorcode4[3] = self.surfGrpAlpha;
    kemoview_set_mesh_color_code(SURF_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
};

- (IBAction)SetSurfGrpPatchColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsSurfGrpNodeColor = [surfGrpPatchColorWell color];
	[nsSurfGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(SURF_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}
- (IBAction)SetSurfGrpLineColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsSurfGrpGridColor = [surfGrpGridColorWell color];
	[nsSurfGrpGridColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(SURF_GRP_FLAG, SURFGRID_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}
- (IBAction)SetSurfGrpNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsSurfGrpNodeColor = [surfGrpNodeColorWell color];
	[nsSurfGrpNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(SURF_GRP_FLAG, SURFNOD_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}
@end

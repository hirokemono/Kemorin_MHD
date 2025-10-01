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
@synthesize eleGrpAlpha;
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

    float colorcode4[4];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_get_mesh_color_code(kemo_sgl, ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4);
    self.eleGrpAlpha = colorcode4[3];

    [ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = kemoview_get_num_of_mesh_group(kemo_sgl, ELEM_GRP_FLAG);
	for(i=0;i<NumElementGroup;i++){
        groupname = kemoview_get_group_name(kemo_sgl, ELEM_GRP_FLAG, i);
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedElementGroupObjectType isEqualToString:@"EleGrpPatch"]) {
		[ElementGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedElementGroupObjectType UTF8String]]);
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i, IONE,
                                        kemo_sgl);
		}
	}
    
    [self UpdateElementTable:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction) HideAllElementGroupAction:(id)pId
{
	int i;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedElementGroupObjectType isEqualToString:@"EleGrpPatch"]) {
		[ElementGroupDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpGrid"]) {
		[ElementGroupDisplayWireFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}
	else if([selectedElementGroupObjectType isEqualToString:@"EleGrpNode"]) {
		[ElementGroupDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumElementGroup;i++){
			[ElementGroupDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
			kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, i, IZERO,
                                        kemo_sgl);
		}
	}	
    [self UpdateElementTable:kemo_sgl];
	[_metalView UpdateImage:kemo_sgl];
}


- (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
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
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    id	identifier = [tableColumn identifier];
    if([identifier isEqualToString:@"EleGrpPatch"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFSOLID_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[ElementGroupDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
    }
    if([identifier isEqualToString:@"EleGrpGrid"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFGRID_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[ElementGroupDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
    if([identifier isEqualToString:@"EleGrpNode"]) {
		kemoview_set_draw_mesh_item(ELEM_GRP_FLAG, SURFNOD_TOGGLE, rowIndex, [object intValue],
                                    kemo_sgl);
		[ElementGroupDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
   }
	[_metalView UpdateImage:kemo_sgl];
}

- (void)tableView:(NSTableView *)aTableView didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedElementGroupObjectType = [tableColumn identifier];
	return;
}

- (void) UpdateElementTable:(struct kemoviewer_type *) kemo_sgl
{
	int i, iflag;
    struct kv_string *groupname;
	NSString *stname;
    float colorcode4[4];
    kemoview_get_mesh_color_code(kemo_sgl, ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4);
    self.eleGrpAlpha = colorcode4[3];

	// printf("Update Element group map\n");
	[ElementGroupDisplayNames removeAllObjects];
	[ElementGroupDisplayPatchFlags removeAllObjects];
	[ElementGroupDisplayWireFlags removeAllObjects];
	[ElementGroupDisplayNodeFlags removeAllObjects];
	NumElementGroup = kemoview_get_num_of_mesh_group(kemo_sgl, ELEM_GRP_FLAG);
	for(i=0;i<NumElementGroup;i++){
        groupname = kemoview_get_group_name(kemo_sgl, ELEM_GRP_FLAG, i);
		stname = [[NSString alloc] initWithUTF8String:groupname->string];
        kemoview_free_kvstring(groupname);

        [ElementGroupDisplayNames      addObject:stname];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, ELEM_GRP_FLAG,
                                            SURFSOLID_TOGGLE, i);
		[ElementGroupDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, ELEM_GRP_FLAG,
                                            SURFGRID_TOGGLE, i);
		[ElementGroupDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		iflag = kemoview_get_draw_mesh_item(kemo_sgl, ELEM_GRP_FLAG,
                                            SURFNOD_TOGGLE, i);
		[ElementGroupDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:iflag] ];
		[stname release];
	}
	[_elementTableView reloadData];

}

- (IBAction)ChooseEleGrpPatchColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpPatchColorItem selectedCell] tag];

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 (int) tag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseEleGrpLineColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpLineColorItem selectedCell] tag];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFGRID_TOGGLE,
                                 (int) tag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)ChooseEleGrpNodeColorAction:(id)sender;
{
	NSInteger tag = [[_EleGrpNodeColorItem selectedCell] tag];
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_mesh_color_flag(ELEM_GRP_FLAG, SURFNOD_TOGGLE,
                                 (int) tag, kemo_sgl);
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetEleGrpPatchAlphaAction:(id)sender
{
    float colorcode4[4];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_get_mesh_color_code(kemo_sgl, ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4);
    colorcode4[3] = self.eleGrpAlpha;
    kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4, kemo_sgl);
    
    [_metalView UpdateImage:kemo_sgl];
};

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
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFSOLID_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
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
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFGRID_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
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
    
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_mesh_color_code(ELEM_GRP_FLAG, SURFNOD_TOGGLE,
                                 colorcode4, kemo_sgl);
	
	[_metalView UpdateImage:kemo_sgl];
}
@end

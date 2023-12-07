//
//  DomainTableController.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 10/09/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "DomainTableController.h"
#include "kemoviewer.h"


@implementation DomainTableController
@synthesize objectDistance;
@synthesize DrawMeshFlag;
@synthesize DomainWindowlabel;

- (id) init
{
 	NumSubDomain = 0;
	DomainDisplayNames= [[NSMutableArray alloc] init];
	DomainDisplayPatchFlags= [[NSMutableArray alloc] init];
	DomainDisplayWireFlags=  [[NSMutableArray alloc] init];
	DomainDisplayNodeFlags=  [[NSMutableArray alloc] init];
	self.DomainWindowlabel = [NSString stringWithFormat:@"Mesh View"];

	return self;
}

- (id) dealloc
{
	[DomainDisplayNames release];
	[DomainDisplayPatchFlags release];
	[DomainDisplayWireFlags  release];
	[DomainDisplayNodeFlags  release];
	[super dealloc];
	return self;
}

- (void) OpenSurfaceMeshFile:(NSString*) MeshOpenFilehead
                    kemoview:(struct kemoviewer_type *) kemo_sgl
{
	int i;
	char name[4096];
	NSString *stname;
	
	self.DomainWindowlabel = [NSString stringWithFormat:@"Mesh View - %@ -",MeshOpenFilehead];
	self.DrawMeshFlag = kemoview_get_draw_mesh_flag(kemo_sgl);
	NumSubDomain =     kemoview_get_num_of_mesh_group(kemo_sgl, DOMAIN_FLAG);

	[DomainDisplayNames removeAllObjects];
	[DomainDisplayPatchFlags removeAllObjects];
	[DomainDisplayWireFlags removeAllObjects];
	[DomainDisplayNodeFlags removeAllObjects];

	for(i=0;i<NumSubDomain;i++){
		sprintf(name,"Domain %d", i);
		stname = [[NSString alloc] initWithUTF8String:name];
		[DomainDisplayNames      addObject:stname];
		[DomainDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
		[DomainDisplayWireFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[DomainDisplayNodeFlags  addObject:[[NSNumber alloc ] initWithInt:0] ];
		[stname release];
	}
	[_tableView reloadData];
    [_nodeGrpController UpdateNodeTable:kemo_sgl];
    [_eleGrpController UpdateElementTable:kemo_sgl];
    [_surfGrpController UpdateSurfaceTable:kemo_sgl];
	
	[_metalView UpdateImage];
}

- (void) ReadMeshFile:(NSString *) MeshOpenFilename
             kemoview:(struct kemoviewer_type *) kemo_sgl
{
    NSString *MeshOpenFilehead = [MeshOpenFilename stringByDeletingPathExtension];
    // NSString *MeshOpenFileheadNoPath = [MeshOpenFilehead lastPathComponent];
    NSString *MeshOpenFileext =  [MeshOpenFilename pathExtension];
    
    if([MeshOpenFileext isEqualToString:@"gz"] || [MeshOpenFileext isEqualToString:@"GZ"]){
        MeshOpenFileext =    [MeshOpenFilehead pathExtension];
        MeshOpenFilehead =   [MeshOpenFilehead stringByDeletingPathExtension];
    };
    
    struct kv_string *filename = kemoview_init_kvstring_by_string([MeshOpenFilehead UTF8String]);
    int iflag_datatype = kemoview_open_data(filename);
    kemoview_free_kvstring(filename);
    
    if(iflag_datatype==IFLAG_MESH ) [self OpenSurfaceMeshFile:MeshOpenFilehead
                                                     kemoview:kemo_sgl];
}

- (IBAction) SelectMeshFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	NSArray *meshFileTypes = [NSArray arrayWithObjects:@"ksm",@"KSM",@"gz",@"GZ",nil];
	NSOpenPanel *MeshOpenPanelObj	= [NSOpenPanel openPanel];
	[MeshOpenPanelObj setTitle:@"Choose mesh data"];
    [MeshOpenPanelObj setAllowedFileTypes:meshFileTypes];
    [MeshOpenPanelObj beginSheetModalForWindow:window 
                              completionHandler:^(NSInteger MeshOpenInteger){
	if(MeshOpenInteger == NSModalResponseOK){
		NSString *MeshOpenFilename = [[MeshOpenPanelObj URL] path];
//		NSString *MeshOpenDirectory = [[MeshOpenPanelObj directoryURL] path];
//      NSLog(@"Mesh file directory = %@",MeshOpenDirectory);
        [self ReadMeshFile:MeshOpenFilename
                  kemoview:kemo_sgl];
	};
                              }];
}

- (IBAction) CloseMeshFile:(id)pId{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_close_mesh_view();

	self.DrawMeshFlag = kemoview_get_draw_mesh_flag(kemo_sgl);
	NumSubDomain = kemoview_get_num_of_mesh_group(kemo_sgl, DOMAIN_FLAG);
	[DomainDisplayNames removeAllObjects];
	[DomainDisplayPatchFlags removeAllObjects];
	[DomainDisplayWireFlags removeAllObjects];
	[DomainDisplayNodeFlags removeAllObjects];

	[_metalView UpdateImage];
}

- (IBAction) ShowAllDomainAction:(id)pId{
	int i;
 
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedDomainObjectType isEqualToString:@"DomainPatch"]) {
		[DomainDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
		}
        kemoview_set_mesh_draw_flag(SURFSOLID_TOGGLE, IONE, kemo_sgl);
	}
	else if([selectedDomainObjectType isEqualToString:@"DomainGrid"]) {
//		NSLog([NSString stringWithFormat:@"select all grid %s",[selectedDomainObjectType UTF8String]]);
		[DomainDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
		}
        kemoview_set_mesh_draw_flag(SURFGRID_TOGGLE, IONE, kemo_sgl);
	}
	else if([selectedDomainObjectType isEqualToString:@"DomainNode"]) {
		[DomainDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:1] ];
		}
        kemoview_set_mesh_draw_flag(SURFNOD_TOGGLE, IONE, kemo_sgl);
	}

	[_metalView UpdateImage];
}

- (IBAction) HideAllDomainAction:(id)pId
{
	int i;
 
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	if([selectedDomainObjectType isEqualToString:@"DomainPatch"]) {
		[DomainDisplayPatchFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayPatchFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
		}
        kemoview_set_mesh_draw_flag(SURFSOLID_TOGGLE, IZERO, kemo_sgl);
	}
	else if([selectedDomainObjectType isEqualToString:@"DomainGrid"]) {
		[DomainDisplayWireFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayWireFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
		}
        kemoview_set_mesh_draw_flag(SURFGRID_TOGGLE, IZERO, kemo_sgl);
	}
	else if([selectedDomainObjectType isEqualToString:@"DomainNode"]) {
		[DomainDisplayNodeFlags removeAllObjects];
		for(i=0;i<NumSubDomain;i++){
			[DomainDisplayNodeFlags addObject:[[NSNumber alloc ] initWithInt:0] ];
		}
        kemoview_set_mesh_draw_flag(SURFNOD_TOGGLE, IZERO, kemo_sgl);
	}

	[_metalView UpdateImage];
}

 - (NSInteger)numberOfRowsInTableView:(NSTableView *)aTableView
{
    return NumSubDomain;
}

- (id)tableView:(NSTableView *)aTableView
objectValueForTableColumn:(NSTableColumn *)aTableColumn
			row:(int)rowIndex
{
	// 'on' の列
	if([[aTableColumn identifier] isEqualToString:@"DomainPatch"]) {
        return [DomainDisplayPatchFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"DomainGrid"]) {
        return [DomainDisplayWireFlags objectAtIndex:rowIndex];
    }
	if([[aTableColumn identifier] isEqualToString:@"DomainNode"]) {
        return [DomainDisplayNodeFlags objectAtIndex:rowIndex];
    }
	// 'name' の列
    else if([[aTableColumn identifier] isEqualToString:@"DomainName"]) {
		return [DomainDisplayNames objectAtIndex:rowIndex];
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
	int iflag;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    identifier = [tableColumn identifier];
    if([identifier isEqualToString:@"DomainPatch"]) {
		[DomainDisplayPatchFlags replaceObjectAtIndex:rowIndex withObject:object];
		iflag = [[DomainDisplayPatchFlags objectAtIndex:rowIndex] intValue];
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFSOLID_TOGGLE, rowIndex, iflag,
                                    kemo_sgl);
    }
    if([identifier isEqualToString:@"DomainGrid"]) {
		[DomainDisplayWireFlags replaceObjectAtIndex:rowIndex withObject:object];
		iflag = [[DomainDisplayWireFlags objectAtIndex:rowIndex] intValue];
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFGRID_TOGGLE, rowIndex, iflag,
                                    kemo_sgl);
    }
    if([identifier isEqualToString:@"DomainNode"]) {
		[DomainDisplayNodeFlags replaceObjectAtIndex:rowIndex withObject:object];
		iflag = [[DomainDisplayNodeFlags objectAtIndex:rowIndex] intValue];
		kemoview_set_draw_mesh_item(DOMAIN_FLAG, SURFNOD_TOGGLE, rowIndex, iflag,
                                    kemo_sgl);
    }

	[_metalView UpdateImage];
}

- (void)tableView:(NSTableView *)aTableView 
didClickTableColumn:(NSTableColumn *)tableColumn
{
	selectedDomainObjectType = [tableColumn identifier];
	return;
}

- (IBAction) UpdateTable:(NSTableView *)aTableView
{
	[aTableView reloadData];
}
- (IBAction)ChooseDomainPatchColorAction:(id)sender;
{
	NSInteger tag = [[_DomainPatchColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFSOLID_TOGGLE,
                                 (int) tag, [_kmv KemoViewPointer]);

	[_metalView UpdateImage];
}

- (IBAction)ChooseDomainLineColorAction:(id)sender;
{
	NSInteger tag = [[_DomainLineColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFGRID_TOGGLE,
                                 (int) tag, [_kmv KemoViewPointer]);

	[_metalView UpdateImage];
}

- (IBAction)ChooseDomainNodeColorAction:(id)sender;
{
	NSInteger tag = [[_DomainNodeColorItem selectedCell] tag];
	kemoview_set_mesh_color_flag(DOMAIN_FLAG, SURFNOD_TOGGLE,
                                 (int) tag, [_kmv KemoViewPointer]);

	[_metalView UpdateImage];
}


- (IBAction)SetDomainPatchColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsDomainNodeColor = [domainPatchColorWell color];
	[nsDomainNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFSOLID_TOGGLE, colorcode4);
	
	[_metalView UpdateImage];
}
- (IBAction)SetDomainLineColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsDomainGridColor = [domainGridColorWell color];
	[nsDomainGridColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFGRID_TOGGLE, colorcode4);
	
	[_metalView UpdateImage];
}
- (IBAction)SetDomainNodeColorAction:(id)sender
{
	CGFloat redBG, greenBG, blueBG, opacityBG;
	float colorcode4[4];
	nsDomainNodeColor = [domainNodeColorWell color];
	[nsDomainNodeColor getRed:&redBG green:&greenBG blue:&blueBG alpha:&opacityBG ];
	colorcode4[0] =  (float) redBG;
	colorcode4[1] =  (float) greenBG;
	colorcode4[2] =  (float) blueBG;
	colorcode4[3] =  (float) opacityBG;
	kemoview_set_mesh_color_code(DOMAIN_FLAG, SURFNOD_TOGGLE, colorcode4);
	
	[_metalView UpdateImage];
}


- (IBAction) SetObjectDistanceAction:(id)pSender
{
	double dblobjectDistance = (double) self.objectDistance;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_domain_distance(dblobjectDistance, kemo_sgl);
	kemoview_draw_with_modified_domain_distance();

	[_metalView UpdateImage];
}
@end

//
//  OpacityMapController.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "OpacityMapController.h"
#include "kemoviewer.h"


@implementation OpacityMapController
@synthesize OpacityTableField;
@synthesize OpacityTableOpacity;
@synthesize idOpacityTableView;

- (void)awakeFromNib {
	self.OpacityTableField =   [[NSMutableArray alloc]init];
	self.OpacityTableOpacity = [[NSMutableArray alloc]init];	
} // end awakeFromNib

- (IBAction)addAtSelectedRow:(id)pId {
	double value, opacity;
	double value1, opacity1;
	double value2, opacity2;
    
	NSInteger isel = [idOpacityTableView selectedRow];
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];

    int n_opacity = kemoview_get_viz_colormap_param(kemo_sgl, id_model,
                                                    ISET_NUM_OPACITY);
    if(n_opacity > 16) return;

	if (isel > 0) {
		value1 =   [[self.OpacityTableField objectAtIndex:isel-1] doubleValue];
		opacity1 = [[self.OpacityTableOpacity objectAtIndex:isel-1] doubleValue];
		value2 =   [[self.OpacityTableField objectAtIndex:isel] doubleValue];
		opacity2 = [[self.OpacityTableOpacity objectAtIndex:isel] doubleValue];
		value =   (value1 + value2)*HALF;
		opacity = (opacity1 + opacity2)*HALF;
        kemoview_add_VIZ_opacity_list(value, opacity,
                                      id_model, kemo_sgl);
		
        [self SetOpacityTables:kemo_sgl];
	}
    [_metalView UpdateImage:kemo_sgl];
    return;
}

- (IBAction)deleteSelectedRow:(id)pId {
    int i;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    NSIndexSet *SelectedList = [idOpacityTableView selectedRowIndexes];
    if([self.OpacityTableField count] < 3) return;
    
    if ([idOpacityTableView numberOfSelectedRows] > 0) {
        for(i = (int) [self.OpacityTableField count]-1;i>1;i--){
            if([SelectedList containsIndex:i] == TRUE){
                kemoview_delete_VIZ_opacity_list(i, id_model, kemo_sgl);
            }
        }
    }
    
    [self SetOpacityTables:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
    return;
}



- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
	return [self.OpacityTableField count];
} // end numberOfRowsInTableView

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex {

	if ([[pTableColumn identifier] isEqualToString:@"Data"]) {
		return [self.OpacityTableField objectAtIndex:pRowIndex];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Opacity"]) {
		return [self.OpacityTableOpacity objectAtIndex:pRowIndex];
	}
	
	NSLog(@"***ERROR** dropped through pTableColumn identifiers");
	return NULL;
	
} // end tableView:objectValueForTableColumn:row:

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject forTableColumn:(NSTableColumn *)pTableColumn row:(NSUInteger)pRowIndex {
	double value, opacity;
	double value1;
	double value2;

	NSUInteger numberOfRaw = [self.OpacityTableField count];
	
	value =    [[self.OpacityTableField objectAtIndex:pRowIndex] doubleValue];
	opacity =  [[self.OpacityTableOpacity objectAtIndex:pRowIndex] doubleValue];
	if(pRowIndex > 0){
		value1 =   [[self.OpacityTableField objectAtIndex:(pRowIndex-1)] doubleValue];
	}
	if(pRowIndex < numberOfRaw-1){
		value2 =   [[self.OpacityTableField objectAtIndex:(pRowIndex+1)] doubleValue];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Data"]) {
		value = [(NSString *)pObject doubleValue];
		if(pRowIndex > 0 && value < value1)               value = value1;
		if(pRowIndex < (numberOfRaw-1) && value > value2) value = value2;
		
		[self.OpacityTableField replaceObjectAtIndex:pRowIndex
										  withObject:[[NSNumber alloc] initWithDouble:value]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Opacity"]) {
		opacity = [(NSString *)pObject doubleValue];
		[self.OpacityTableOpacity replaceObjectAtIndex:pRowIndex
										  withObject:[[NSNumber alloc] initWithDouble:opacity]];
	}
	
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_opacity_data((int) pRowIndex, value, opacity,
                                  id_model, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
	[_fillRectView UpdateColorbar];
    return;
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void) SetOpacityTables:(struct kemoviewer_type *) kemo_sgl
{
	int i;
	double value, opacity;
	
	[OpacityTableField removeAllObjects];
	[OpacityTableOpacity removeAllObjects];
    int id_model = (int) [_kemoviewControl CurrentControlModel];
	NumOpacityTable = kemoview_get_viz_colormap_param(kemo_sgl, id_model,
                                                      ISET_NUM_OPACITY);
	for(i=0;i<NumOpacityTable;i++){
		kemoview_get_PSF_opacity_items(kemo_sgl, id_model,
                                       i, &value, &opacity);
		[OpacityTableField    addObject:[[NSNumber alloc ] initWithDouble:value] ];
		[OpacityTableOpacity addObject:[[NSNumber alloc ] initWithDouble:opacity] ];
	}
	[_OpacityTableView reloadData];
}

- (IBAction) UpdateOpacityTables:(id)pID
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetOpacityTables:kemo_sgl];
	[_fillRectView UpdateColorbar];
}


@end

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

@synthesize DataMaximum;
@synthesize DataMinimum;

- (void)awakeFromNib {
	self.OpacityTableField =   [[NSMutableArray alloc]init];
	self.OpacityTableOpacity = [[NSMutableArray alloc]init];	
} // end awakeFromNib

- (IBAction)addAtSelectedRow:(id)pId {
	double value, color, opacity;
	double value1, opacity1;
	double value2, opacity2;
	int i;
	int isel = [idOpacityTableView selectedRow];

	if ([idOpacityTableView selectedRow] > 0) {
		value = 0.5;
		color = 0.5;
		opacity = 1.0;
		
		value1 =   [[self.OpacityTableField objectAtIndex:isel-1] doubleValue];
		opacity1 = [[self.OpacityTableOpacity objectAtIndex:isel-1] doubleValue];
		value2 =   [[self.OpacityTableField objectAtIndex:isel] doubleValue];
		opacity2 = [[self.OpacityTableOpacity objectAtIndex:isel] doubleValue];
		value =   (value1 + value2)*HALF;
		opacity = (opacity1 + opacity2)*HALF;
		
		[OpacityTableField   insertObject:[[NSNumber alloc] initWithDouble:value] atIndex:isel];
		[OpacityTableOpacity insertObject:[[NSNumber alloc] initWithDouble:opacity] atIndex:isel];
		[idOpacityTableView reloadData];
		
		int num = [self.OpacityTableField count];
		realloc_current_PSF_opacity_idx_list(num);
		for (i=0;i<num;i++){
			value =   [[self.OpacityTableField objectAtIndex:i] doubleValue];
			opacity = [[self.OpacityTableOpacity objectAtIndex:i] doubleValue];
			set_current_PSF_opacity_point(i, value, opacity);
		}
	}
	
} // end deleteSelectedRow

- (IBAction)deleteSelectedRow:(id)pId {
	double value, opacity;
	int i;
	
	NSIndexSet *SelectedList = [idOpacityTableView selectedRowIndexes];
	
	if ([idOpacityTableView numberOfSelectedRows] > 0) {
		for(int i=[self.OpacityTableField count]-2;i>1;i--){
			if([SelectedList containsIndex:i] !=0){
				[OpacityTableField    removeObjectAtIndex:i];
				[OpacityTableOpacity removeObjectAtIndex:i];
			}
		}
		[idOpacityTableView reloadData];
		
		int num = [self.OpacityTableField count];
		realloc_current_PSF_opacity_idx_list(num);
		for (i=0;i<num;i++){
			value =   [[self.OpacityTableField objectAtIndex:i] doubleValue];
			opacity = [[self.OpacityTableOpacity objectAtIndex:i] doubleValue];
			set_current_PSF_opacity_point(i, value, opacity);
		}
	}
}



- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
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

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject forTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex {
	double value, opacity;
	double value1;
	double value2;

	int numberOfRaw = [self.OpacityTableField count];
	
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
	
	set_current_PSF_opacity_point(pRowIndex, value, opacity);
	
	[_kemoviewer UpdateImage];
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)InitOpacityTables {
	double d_min, d_max;
	
	NumOpacityTable = 2;
	d_min = send_current_PSF_color_table_min();
	d_max = send_current_PSF_color_table_max();
	realloc_current_PSF_opacity_idx_list(NumOpacityTable);
	set_current_PSF_opacity_point(IZERO, d_min, ONE);
	set_current_PSF_opacity_point(IONE,  d_max, ONE);
	
	[self SetOpacityTables];
}

- (void) SetOpacityTables;
{
	int i;
	double value, opacity;
	
	[OpacityTableField removeAllObjects];
	[OpacityTableOpacity removeAllObjects];
	NumOpacityTable = send_current_PSF_opacity_table_num();
	for(i=0;i<NumOpacityTable;i++){
		send_current_PSF_opacity_table_items(i, &value, &opacity);
		[OpacityTableField    addObject:[[NSNumber alloc ] initWithDouble:value] ];
		[OpacityTableOpacity addObject:[[NSNumber alloc ] initWithDouble:opacity] ];
	}
	[_OpacityTableView reloadData];
}

- (IBAction) UpdateOpacityTables:(id)pID
{
	[self SetOpacityTables];
	[_fillRectView UpdateColorbar];
}


@end

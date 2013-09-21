/*
//
//  ColorMapController.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/22.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//
*/

#import "ColorMapController.h"
#import "kemoviewer.h"


@implementation ColorMapController
@synthesize ColorTableField;
@synthesize ColorTableColor;
@synthesize idColorTableView;

- (void)awakeFromNib {
	float r, g, b;
	
	self.ColorTableField = [[NSMutableArray alloc]init];
	self.ColorTableColor = [[NSMutableArray alloc]init];
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	r = [[defaults stringForKey:@"BackGroundRed"] floatValue];
	g = [[defaults stringForKey:@"BackGroundGreen"] floatValue];
	b = [[defaults stringForKey:@"BackGroundBlue"] floatValue];

	NSColor *theColor = [NSColor colorWithDeviceRed:r green:g blue:b alpha:1.0];
	[backgroundColorWell setColor:theColor];
} // end awakeFromNib

- (IBAction)addAtSelectedRow:(id)pId {
	double value, color;
	double value1, color1;
	double value2, color2;
	int i;
	int isel = [idColorTableView selectedRow];
	
	if ([idColorTableView selectedRow] > 0) {
		value = 0.5;
		color = 0.5;
		color = 1.0;
		
		value1 =   [[self.ColorTableField objectAtIndex:isel-1] doubleValue];
		color1 = [[self.ColorTableColor objectAtIndex:isel-1] doubleValue];
		value2 =   [[self.ColorTableField objectAtIndex:isel] doubleValue];
		color2 = [[self.ColorTableColor objectAtIndex:isel] doubleValue];
		value = (value1 + value2)*HALF;
		color = (color1 + color2)*HALF;
		
		[ColorTableField insertObject:[[NSNumber alloc] initWithDouble:value] atIndex:isel];
		[ColorTableColor insertObject:[[NSNumber alloc] initWithDouble:color] atIndex:isel];
		[idColorTableView reloadData];
		
		int num = [self.ColorTableField count];
		realloc_current_PSF_color_idx_list(RAINBOW_MODE, num);
		for (i=0;i<num;i++){
			value =   [[self.ColorTableField objectAtIndex:i] doubleValue];
			color = [[self.ColorTableColor objectAtIndex:i] doubleValue];
			set_current_PSF_color_point(i, value, color);
		}
	}
	[_kemoviewer UpdateImage];
} // end deleteSelectedRow

- (IBAction)deleteSelectedRow:(id)pId {
	double value, color;
	int i;
	
	NSIndexSet *SelectedList = [idColorTableView selectedRowIndexes];
	
	if ([idColorTableView numberOfSelectedRows] > 0) {
		for(int i=[self.ColorTableField count]-2;i>1;i--){
			if([SelectedList containsIndex:i] !=0){
				[ColorTableField    removeObjectAtIndex:i];
				[ColorTableColor removeObjectAtIndex:i];
			}
		}
		[idColorTableView reloadData];
		
		int num = [self.ColorTableField count];
		realloc_current_PSF_color_idx_list(RAINBOW_MODE, num);
		for (i=0;i<num;i++){
			value = [[self.ColorTableField objectAtIndex:i] doubleValue];
			color = [[self.ColorTableColor objectAtIndex:i] doubleValue];
			set_current_PSF_color_point(i, value, color);
		}
	}
	[_kemoviewer UpdateImage];
}



- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
	return [self.ColorTableField count];
} // end numberOfRowsInTableView

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex {
	
	if ([[pTableColumn identifier] isEqualToString:@"Data"]) {
		return [self.ColorTableField objectAtIndex:pRowIndex];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Color"]) {
		return [self.ColorTableColor objectAtIndex:pRowIndex];
	}
	
	NSLog(@"***ERROR** dropped through pTableColumn identifiers");
	return NULL;
	
} // end tableView:objectValueForTableColumn:row:

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject forTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex {
	double value, color;
	double value1;
	double value2;
	
	int numberOfRaw = [self.ColorTableField count];
	
	value =  [[self.ColorTableField objectAtIndex:pRowIndex] doubleValue];
	color =  [[self.ColorTableColor objectAtIndex:pRowIndex] doubleValue];
	if(pRowIndex > 0){
		value1 =   [[self.ColorTableField objectAtIndex:(pRowIndex-1)] doubleValue];
	}
	if(pRowIndex < numberOfRaw-1){
		value2 =   [[self.ColorTableField objectAtIndex:(pRowIndex+1)] doubleValue];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Data"]) {
		value = [(NSString *)pObject doubleValue];
		if(pRowIndex > 0 && value < value1)               value = value1;
		if(pRowIndex < (numberOfRaw-1) && value > value2) value = value2;
		
		[self.ColorTableField replaceObjectAtIndex:pRowIndex
										  withObject:[[NSNumber alloc] initWithDouble:value]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Color"]) {
		color = [(NSString *)pObject doubleValue];
		[self.ColorTableColor replaceObjectAtIndex:pRowIndex
											withObject:[[NSNumber alloc] initWithDouble:color]];
	}
	
	set_current_PSF_color_point(pRowIndex, value, color);

	[_kemoviewer UpdateImage];
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)InitColorTables;
{
	double d_min, d_max;
	
	NumColorTable = 2;
	d_min = send_current_PSF_color_table_min();
	d_max = send_current_PSF_color_table_max();
	
	[self SetColorTables];
}

- (void)SetColorTables;
{
	int i;
	double value, color;
	//	double value, color, color;
	
	[ColorTableField removeAllObjects];
	[ColorTableColor removeAllObjects];
	NumColorTable = send_current_PSF_color_table_num();
	for(i=0;i<NumColorTable;i++){
		send_current_PSF_color_table_items(i, &value, &color);
		[ColorTableField addObject:[[NSNumber alloc ] initWithDouble:value] ];
		[ColorTableColor addObject:[[NSNumber alloc ] initWithDouble:color] ];
	}
	[_colorTableView reloadData];
}

- (IBAction)UpdateColorTables:(id)pID
{
	[self SetColorTables];
	[_fillRectView UpdateColorbar];
}

- (IBAction)ChooseBackgroundColorAction: (id) sender;
{
    int i;
    CGFloat rgbaBG[4];
    GLfloat glrgbaBG[4];
	
	nsBackgroundColor = [backgroundColorWell color];
	[nsBackgroundColor getRed:&rgbaBG[0] green:&rgbaBG[1] blue:&rgbaBG[2] alpha:&rgbaBG[3] ];
    for (i=0; i<3; i++) glrgbaBG[i] = (GLfloat) rgbaBG[i];
	glrgbaBG[3] = ONE;

	set_kemoview_background_color(glrgbaBG);

	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) rgbaBG[0]) forKey:@"BackGroundRed"];
	[defaults setFloat:((float) rgbaBG[1]) forKey:@"BackGroundGreen"];
	[defaults setFloat:((float) rgbaBG[2]) forKey:@"BackGroundBlue"];
	
	[_kemoviewer swapbuffer_cocoa];
}

@end

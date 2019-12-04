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
	int isel = [idColorTableView selectedRow];
	
	if(isel > 0) {
		value1 = [[self.ColorTableField objectAtIndex:isel-1] doubleValue];
		color1 = [[self.ColorTableColor objectAtIndex:isel-1] doubleValue];
		value2 = [[self.ColorTableField objectAtIndex:isel] doubleValue];
		color2 = [[self.ColorTableColor objectAtIndex:isel] doubleValue];
		value = (value1 + value2)*HALF;
		color = (color1 + color2)*HALF;
		kemoview_add_PSF_color_list(value, color);
		
		[self SetColorTables];
	}
//	[_kemoviewer UpdateImage];
}

- (IBAction)deleteSelectedRow:(id)pId {
	int i;
	
	NSIndexSet *SelectedList = [idColorTableView selectedRowIndexes];
	if([self.ColorTableField count] < 3) return;
	
	if ([idColorTableView numberOfSelectedRows] > 0) {
		for(i = [self.ColorTableField count]-1;i>1;i--){
			if([SelectedList containsIndex:i] == TRUE){
				kemoview_delete_PSF_color_list(i);
			}
		};
	}
	
	[self SetColorTables];
//	[_kemoviewer UpdateImage];
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
	
	kemoview_set_PSF_color_data(pRowIndex, value, color);

//	[_kemoviewer UpdateImage];
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)InitColorTables;
{
	double d_min, d_max;
	
	NumColorTable = 2;
	d_min = kemoview_get_each_PSF_colormap_range(ISET_COLOR_MIN);
	d_max = kemoview_get_each_PSF_colormap_range(ISET_COLOR_MAX);
	
	[self SetColorTables];
}

- (void)SetColorTables;
{
	int i;
	double value, color;
	//	double value, color, color;
	
	[ColorTableField removeAllObjects];
	[ColorTableColor removeAllObjects];
	NumColorTable = kemoview_get_PSF_color_param(ISET_NUM_COLOR);
	for(i=0;i<NumColorTable;i++){
		kemoview_get_PSF_color_items(i, &value, &color);
		[ColorTableField addObject:[[NSNumber alloc ] initWithDouble:value] ];
		[ColorTableColor addObject:[[NSNumber alloc ] initWithDouble:color] ];
	}
	[_colorTableView reloadData];

	[ColorModeItem selectItemAtIndex:kemoview_get_PSF_color_param(ISET_COLORMAP)];
}

- (IBAction)UpdateColorTables:(id)pID
{
	[self SetColorTables];
//	[_fillRectView UpdateColorbar];
}

- (IBAction)ChooseBackgroundColorAction: (id) sender;
{
    int i;
    CGFloat rgbaBG[4];
    float glrgbaBG[4];
	
	nsBackgroundColor = [backgroundColorWell color];
	[nsBackgroundColor getRed:&rgbaBG[0] green:&rgbaBG[1] blue:&rgbaBG[2] alpha:&rgbaBG[3] ];
    for (i=0; i<3; i++) glrgbaBG[i] = (float) rgbaBG[i];
	glrgbaBG[3] = ONE;

	kemoview_set_background_color(glrgbaBG);

	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) rgbaBG[0]) forKey:@"BackGroundRed"];
	[defaults setFloat:((float) rgbaBG[1]) forKey:@"BackGroundGreen"];
	[defaults setFloat:((float) rgbaBG[2]) forKey:@"BackGroundBlue"];
	
	[_kemoviewer swapbuffer_cocoa];
}

- (IBAction)SetColorMode:(id)pId;
{
	kemoview_set_PSF_color_param(ISET_COLORMAP, (int) [ColorModeItem indexOfSelectedItem]);
	[_kemoviewer UpdateImage];
}


@end

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
//@synthesize _colorModeItem;

- (id)init{
    [super init];
    _colorModeItem = [[NSPopUpButton alloc] init];
    return self;
}
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
	NSInteger isel = [idColorTableView selectedRow];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    
    if(kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_COLOR) > 16) return;
        
	if(isel > 0) {
		value1 = [[self.ColorTableField objectAtIndex:isel-1] doubleValue];
		color1 = [[self.ColorTableColor objectAtIndex:isel-1] doubleValue];
		value2 = [[self.ColorTableField objectAtIndex:isel] doubleValue];
		color2 = [[self.ColorTableColor objectAtIndex:isel] doubleValue];
		value = (value1 + value2)*HALF;
		color = (color1 + color2)*HALF;
		kemoview_add_PSF_color_list(value, color, kemo_sgl);
		
        [self SetColorTables:kemo_sgl];
	}
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)deleteSelectedRow:(id)pId {
	int i;
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	NSIndexSet *SelectedList = [idColorTableView selectedRowIndexes];
	if([self.ColorTableField count] < 3) return;
	
	if ([idColorTableView numberOfSelectedRows] > 0) {
		for(i = (int) [self.ColorTableField count]-1;i>1;i--){
			if([SelectedList containsIndex:i] == TRUE){
				kemoview_delete_PSF_color_list(i, kemo_sgl);
			}
		};
	}
	
    [self SetColorTables:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}



- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
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
	
	int numberOfRaw = (int) [self.ColorTableField count];
	
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
	
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_data(pRowIndex, value, color, kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
	[_fillRectView UpdateColorbar];
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}

- (void)InitColorTables:(struct kemoviewer_type *) kemo_sgl
{
	double d_min, d_max;
	
	NumColorTable = 2;
	d_min = kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_COLOR_MIN);
	d_max = kemoview_get_each_PSF_colormap_range(kemo_sgl, ISET_COLOR_MAX);
	
    [self SetColorTables:kemo_sgl];
}

- (void)SetColorTables:(struct kemoviewer_type *) kemo_sgl
{
	int i;
	double value, color;
	//	double value, color, color;
	
	[ColorTableField removeAllObjects];
	[ColorTableColor removeAllObjects];
	NumColorTable = kemoview_get_PSF_color_param(kemo_sgl, ISET_NUM_COLOR);
	for(i=0;i<NumColorTable;i++){
		kemoview_get_PSF_color_items(kemo_sgl, i, &value, &color);
		[ColorTableField addObject:[[NSNumber alloc ] initWithDouble:value] ];
		[ColorTableColor addObject:[[NSNumber alloc ] initWithDouble:color] ];
	}
	[_colorTableView reloadData];

	[_colorModeItem selectItemAtIndex:kemoview_get_PSF_color_param(kemo_sgl, ISET_COLORMAP)];
    return;
}

- (IBAction)UpdateColorTables:(id)pID
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetColorTables:kemo_sgl];
	[_fillRectView UpdateColorbar];
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

    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_background_color(glrgbaBG, kemo_sgl);

	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) rgbaBG[0]) forKey:@"BackGroundRed"];
	[defaults setFloat:((float) rgbaBG[1]) forKey:@"BackGroundGreen"];
	[defaults setFloat:((float) rgbaBG[2]) forKey:@"BackGroundBlue"];
	
    [_metalView updateBackground:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}

- (IBAction)SetColorMode:(id)pId;
{
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	kemoview_set_PSF_color_param(ISET_COLORMAP, 
                                 (int) [_colorModeItem indexOfSelectedItem],
                                 kemo_sgl);
	[_fillRectView UpdateColorbar];
    [_metalView UpdateImage:kemo_sgl];
}


@end

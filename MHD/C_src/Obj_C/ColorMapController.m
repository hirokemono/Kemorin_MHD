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


- (void)SetColorTables:(int) id_model
              kemoview:(struct kemoviewer_type *) kemo_sgl
{
    int i;
    double value, color;
    //    double value, color, color;
    int currentStep = [_kemoviewControl SetCurrentPSFFile:id_model
                                                 kemoview:kemo_sgl
                                                 pathTree:_cmapPathControl];

    [ColorTableField removeAllObjects];
    [ColorTableColor removeAllObjects];
    NumColorTable = kemoview_get_viz_colormap_param(kemo_sgl, id_model,
                                                    ISET_NUM_COLOR);
    for(i=0;i<NumColorTable;i++){
        kemoview_get_VIZ_color_RGB_value(kemo_sgl, id_model,
                                         i, &value, &color);
        [ColorTableField addObject:[[NSNumber alloc ] initWithDouble:value] ];
        [ColorTableColor addObject:[[NSNumber alloc ] initWithDouble:color] ];
    }
    [_colorTableView reloadData];

    [_colorModeItem selectItemAtIndex:kemoview_get_viz_colormap_param(kemo_sgl, id_model,
                                                                      ISET_COLORMAP)];
    return;
}

- (IBAction)UpdateColorTables:(id)pID
{
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    [self SetColorTables:id_model
                kemoview:kemo_sgl];
    [_fillRectView UpdateColorbar];
}


- (IBAction)addAtSelectedRow:(id)pId {
	double value, color;
	double value1, color1;
	double value2, color2;
	NSInteger isel = [idColorTableView selectedRow];
    
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    
    int n_color = kemoview_get_viz_colormap_param(kemo_sgl, id_model,
                                                  ISET_NUM_COLOR);
    if(n_color > 16) return;
        
	if(isel > 0) {
		value1 = [[self.ColorTableField objectAtIndex:isel-1] doubleValue];
		color1 = [[self.ColorTableColor objectAtIndex:isel-1] doubleValue];
		value2 = [[self.ColorTableField objectAtIndex:isel] doubleValue];
		color2 = [[self.ColorTableColor objectAtIndex:isel] doubleValue];
		value = (value1 + value2)*HALF;
		color = (color1 + color2)*HALF;
        kemoview_add_VIZ_color_list(value, color,
                                    id_model, kemo_sgl);
		
        [self SetColorTables:id_model
                    kemoview:kemo_sgl];
	}
	[_metalView UpdateImage:kemo_sgl];
}

- (IBAction)deleteSelectedRow:(id)pId {
	int i;
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
	NSIndexSet *SelectedList = [idColorTableView selectedRowIndexes];
	if([self.ColorTableField count] < 3) return;
	
	if ([idColorTableView numberOfSelectedRows] > 0) {
		for(i = (int) [self.ColorTableField count]-1;i>1;i--){
			if([SelectedList containsIndex:i] == TRUE){
                kemoview_delete_VIZ_color_list(i, id_model, kemo_sgl);
			}
		};
	}
	
    [self SetColorTables:id_model
                kemoview:kemo_sgl];
    [_metalView UpdateImage:kemo_sgl];
}



- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj {
	return [self.ColorTableField count];
} // end numberOfRowsInTableView

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn
             row:(int)pRowIndex {
	
	if ([[pTableColumn identifier] isEqualToString:@"Data"]) {
		return [self.ColorTableField objectAtIndex:pRowIndex];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Color"]) {
		return [self.ColorTableColor objectAtIndex:pRowIndex];
	}
	
	NSLog(@"***ERROR** dropped through pTableColumn identifiers");
	return NULL;
	
} // end tableView:objectValueForTableColumn:row:

- (void)tableView:(NSTableView *)pTableViewObj
   setObjectValue:(id)pObject
   forTableColumn:(NSTableColumn *)pTableColumn
              row:(int)pRowIndex {
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
	
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_VIZ_color_point(pRowIndex, value, color,
                                 id_model,
                                 kemo_sgl);
    [_metalView UpdateImage:kemo_sgl];
	[_fillRectView UpdateColorbar];
} // end tableView:setObjectValue:forTableColumn:row:

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj
 objectValueForTableColumn:(NSTableColumn *)pTableColumn
                       row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
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
    int id_model = (int) [_kemoviewControl CurrentControlModel];
    struct kemoviewer_type *kemo_sgl = [_kmv KemoViewPointer];
    kemoview_set_colormap_param(id_model, ISET_COLORMAP,
                                (int) [_colorModeItem indexOfSelectedItem],
                                kemo_sgl);
	[_fillRectView UpdateColorbar];
    [_metalView UpdateImage:kemo_sgl];
}


@end

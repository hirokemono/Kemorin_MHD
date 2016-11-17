//
//  RGBAMapController.m
//  025-NSTableView
//
//  Created by Hiroaki Matsui on 11/08/23.
//  Copyright 2011 Dept. of Earth and Planetary Science, UC Berkeley. All rights reserved.
//

#import "RGBAMapController.h"
#include "kemoviewer.h"


@implementation RGBAMapController
@synthesize DataMaximum;
@synthesize DataMinimum;

- (void)awakeFromNib {
	self.DataMinimum = ZERO;
	self.DataMaximum = ONE;
}

- (void)updateColormapParameter {
	self.DataMinimum = send_current_PSF_color_table_min();
	self.DataMaximum = send_current_PSF_color_table_max();
}

- (IBAction)SetColorMode:(id)pId;
{
	int iflag_colormode;
	iflag_colormode = [ColorModeItem indexOfSelectedItem];
	set_current_PSF_color_mode_id(iflag_colormode);

	[_kemoviewer UpdateImage];
}

- (IBAction) SetColormapMinMax:(id)pSender {
	[_colorMapObject InitColorTables];
	[_colorMapObject SetColorTables];
	[_opacityMapObject InitOpacityTables];
	[_opacityMapObject SetOpacityTables];
}


- (IBAction) SaveColormapFile:(id)pId;{
	
	NSSavePanel *ViewMatrixSavePanelObj	= [NSSavePanel savePanel];
	int ViewMatrixSaveInt = [ViewMatrixSavePanelObj runModal];
	if(ViewMatrixSaveInt == NSFileHandlingPanelOKButton){
		
		NSString * ViewMatrixFilename = [[ ViewMatrixSavePanelObj URL] path];
		NSString * ViewMatrixDirectory = [[ ViewMatrixSavePanelObj directoryURL] path];
		NSString * ViewMatrixFilehead = [ ViewMatrixFilename stringByDeletingPathExtension];
		NSLog(@" ViewMatrixFilename = %@",  ViewMatrixFilename);
		NSLog(@" ViewMatrixDirectory = %@", ViewMatrixDirectory);
		NSLog(@" ViewMatrixFilehead = %@",  ViewMatrixFilehead);
		
		write_current_PSF_colormap_control_file([ViewMatrixFilename UTF8String]);
	};
}

@end

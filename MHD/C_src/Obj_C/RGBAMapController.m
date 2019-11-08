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
	self.DataMinimum = kemoview_get_each_PSF_colormap_range(ISET_COLOR_MIN);
	self.DataMaximum = kemoview_get_each_PSF_colormap_range(ISET_COLOR_MAX);
}

- (void) SetColormapMinMax{
	[_colorMapObject InitColorTables];
	[_colorMapObject SetColorTables];
	[_opacityMapObject InitOpacityTables];
	[_opacityMapObject SetOpacityTables];
}


- (IBAction) SaveColormapFile:(id)pId;{
	
	NSSavePanel *ColormapSavePanelObj	= [NSSavePanel savePanel];
    [ColormapSavePanelObj beginSheetModalForWindow:window 
                                    completionHandler:^(NSInteger ColrmapSaveInt){
	if(ColrmapSaveInt == NSFileHandlingPanelOKButton){
		
		NSString * ColormapFilename = [[ ColormapSavePanelObj URL] path];
		NSString * ColormapDirectory = [[ ColormapSavePanelObj directoryURL] path];
		NSString * ColormapFilehead = [ ColormapFilename stringByDeletingPathExtension];
		NSLog(@" ColormapFilename = %@",  ColormapFilename);
		NSLog(@" ColormapDirectory = %@", ColormapDirectory);
		NSLog(@" ColormapFilehead = %@",  ColormapFilehead);
		
        struct kv_string *filename = kemoview_init_kvstring_by_string([ColormapFilename UTF8String]);
		kemoview_write_PSF_colormap_file(filename);
        kemoview_free_kvstring(filename);
	};
                                    }];
}

- (IBAction) LoadColormapFile:(id)pId;{
    NSArray *ColormapFileTypes = [NSArray arrayWithObjects:@"dat",@"DAT",nil];
    
    NSOpenPanel *ColormapOpenPanelObj	= [NSOpenPanel openPanel];
    [ColormapOpenPanelObj setTitle:@"Choose colormap data"];
    [ColormapOpenPanelObj setAllowedFileTypes:ColormapFileTypes];
    [ColormapOpenPanelObj beginSheetModalForWindow:window 
                                   completionHandler:^(NSInteger ColormapOpenInteger){
                                       if(ColormapOpenInteger == NSFileHandlingPanelOKButton){
                                           
                                           NSString * ColormapFilename = [[ ColormapOpenPanelObj URL] path];
                                           NSString * ColormapDirectory = [[ ColormapOpenPanelObj directoryURL] path];
                                           NSString * ColormapFilehead = [ ColormapFilename stringByDeletingPathExtension];
                                           NSLog(@" ColormapFilename = %@",  ColormapFilename);
                                           NSLog(@" ColormapDirectory = %@", ColormapDirectory);
                                           NSLog(@" ColormapFilehead = %@",  ColormapFilehead);
                                           
                                           struct kv_string *filename = kemoview_init_kvstring_by_string([ColormapFilename UTF8String]);
                                           kemoview_read_PSF_colormap_file(filename);
                                           kemoview_free_kvstring(filename);
                                           
                                           [_kemoviewer UpdateImage];
                                           [_colorMapObject SetColorTables];
                                           [_opacityMapObject SetOpacityTables];
                                       };
                                   }];
    
}

@end

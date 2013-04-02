//
//  Kemoview_IO_Controller.m
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "Kemoview_IO_Controller.h"

#include "kemoviewer.h"


@implementation Kemoview_IO_Controller
@synthesize ImageFormatFlag;
- (void) OpenKemoviewerFile:(NSString*) kemoviewOpenFilename
{
	int iflag_filetype;
	NSString *kemoviewOpenFilehead = [kemoviewOpenFilename stringByDeletingPathExtension];
	NSString *KemoviewOpenFileext =  [kemoviewOpenFilename pathExtension];
	
	if([KemoviewOpenFileext isEqualToString:@"gz"] || [KemoviewOpenFileext isEqualToString:@"GZ"]){
		KemoviewOpenFileext =    [kemoviewOpenFilehead pathExtension];
		kemoviewOpenFilehead =   [kemoviewOpenFilehead stringByDeletingPathExtension];
		if([KemoviewOpenFileext isEqualToString:@"0"]){
/*			iflag_filetype == IFLAG_FULL_MESH_GZ;*/
			return;
		};
	}

	[_fileReadBar setUsesThreadedAnimation:YES];
	[_fileReadBar startAnimation:self];
	iflag_filetype = kemoview_open_data_glut((char *) [kemoviewOpenFilename UTF8String]);
	[_fileReadBar stopAnimation:self];
	
	if(iflag_filetype==IFLAG_SURF_MESH || iflag_filetype==IFLAG_SURF_MESH_GZ) {
		[_domainTableController OpenSurfaceMeshFile:kemoviewOpenFilehead];
	}
	else if(   iflag_filetype==IFLAG_SURF_UDT || iflag_filetype==IFLAG_SURF_UDT_GZ
			|| iflag_filetype==IFLAG_SURF_UCD || iflag_filetype==IFLAG_SURF_UCD_GZ) {
		[_psfController DrawPsfFile:kemoviewOpenFilehead];
		[_movieMakerController InitEvolutionStepByPSF];
	}
	else if(iflag_filetype==IFLAG_LINE_UCD || iflag_filetype==IFLAG_LINE_UCD_GZ) {
		[_flineController OpenFieldlineFile:kemoviewOpenFilehead];
		[_movieMakerController InitEvolutionStepByFline];
	};
	
}

- (IBAction) OpenKemoviewerFileByMenu:(id)pId;
{
	NSArray *kemoviewFileTypes = [NSArray arrayWithObjects:@"ksm",@"KSM",@"udt",@"UDT",@"inp",@"INP",
							  @"gz",@"GZ",@"0",nil];
	NSOpenPanel *KemoviewOpenPanelObj	= [NSOpenPanel openPanel];
	[KemoviewOpenPanelObj setTitle:@"Choose data for Kemoviewer"];
    [KemoviewOpenPanelObj setAllowedFileTypes:kemoviewFileTypes];
	NSInteger KemoviewOpenInteger	= [KemoviewOpenPanelObj runModal];
	
	if(KemoviewOpenInteger == NSOKButton){
		NSString *kemoviewOpenFilename = [[KemoviewOpenPanelObj URL] path];
		[self OpenKemoviewerFile:kemoviewOpenFilename];
	};	
}

- (IBAction) SaveViewMatrixFile:(id)pId;
{
	NSSavePanel *ViewMatrixSavePanelObj	= [NSSavePanel savePanel];
	int ViewMatrixSaveInt = [ViewMatrixSavePanelObj runModal];
	if(ViewMatrixSaveInt == NSOKButton){
		
		NSString * ViewMatrixFilename = [[ ViewMatrixSavePanelObj URL] path];
		NSString * ViewMatrixDirectory = [[ ViewMatrixSavePanelObj directoryURL] path];
		NSString * ViewMatrixFilehead = [ ViewMatrixFilename stringByDeletingPathExtension];
		NSLog(@" ViewMatrixFilename = %@",  ViewMatrixFilename);
		NSLog(@" ViewMatrixDirectory = %@", ViewMatrixDirectory);
		NSLog(@" ViewMatrixFilehead = %@",  ViewMatrixFilehead);

		write_modelview_file_glut((char *) [ViewMatrixFilename UTF8String]);
	};
}

- (IBAction) LoadViewMatrixFile:(id)pId;
{
	NSArray *MatrixFileTypes = [NSArray arrayWithObjects:@"dat",@"DAT",nil];
    
	NSSavePanel *ViewMatrixOpenPanelObj	= [NSOpenPanel openPanel];
	[ViewMatrixOpenPanelObj setTitle:@"Choose View Matrix data"];
    [ViewMatrixOpenPanelObj setAllowedFileTypes:MatrixFileTypes];
	NSInteger ViewMatrixOpenInteger	= [ViewMatrixOpenPanelObj runModal];

    if(ViewMatrixOpenInteger == NSOKButton){
		
		NSString * ViewMatrixFilename = [[ ViewMatrixOpenPanelObj URL] path];
		NSString * ViewMatrixDirectory = [[ ViewMatrixOpenPanelObj directoryURL] path];
		NSString * ViewMatrixFilehead = [ ViewMatrixFilename stringByDeletingPathExtension];
		NSLog(@" ViewMatrixFilename = %@",  ViewMatrixFilename);
		NSLog(@" ViewMatrixDirectory = %@", ViewMatrixDirectory);
		NSLog(@" ViewMatrixFilehead = %@",  ViewMatrixFilehead);
        
		load_modelview_file_glut((char *) [ViewMatrixFilename UTF8String]);
		[_kemoviewer UpdateImage];
	};
}

- (IBAction) SaveImageFile:(id)pId;{
	NSInteger id_format;

	NSUserDefaults* defaults = [_user_defaults_controller defaults];
	CurrentImageFormat = [[defaults stringForKey:@"ImageFormatID"] intValue];
	
	NSSavePanel *ImageSavePanelObj	= [NSSavePanel savePanel];
	[ImageSavePanelObj setTitle:@"Save Image File"];
	[ImageSavePanelObj setCanSelectHiddenExtension:YES];
	NSInteger ImageSaveInt = [ImageSavePanelObj runModal];
	if(ImageSaveInt == NSOKButton){
		
		// NSString * ImageDirectory = [ ImageSavePanelObj directory];
		NSString * ImageFilename = [[ ImageSavePanelObj URL] path];
		NSString * ImageFilehead =  [ ImageFilename stringByDeletingPathExtension];
		NSString * ImageFileext =   [ImageFilename pathExtension];
		// NSLog(@" ImageFilename = %@",  ImageFilename);
		// NSLog(@" ImageDirectory = %@", ImageDirectory);
		// NSLog(@" ImageFilehead = %@",  ImageFilehead);

		if([ImageFileext isEqualToString:@"png"]) {
			id_format = SAVE_PNG;
		}
		else if([ImageFileext isEqualToString:@"bmp"]){
			id_format = SAVE_BMP;
		}
		else if([ImageFileext isEqualToString:@"eps"]){
			id_format = SAVE_EPS;
		}
		else if([ImageFileext isEqualToString:@"pdf"]){
			id_format = SAVE_PDF;
		}
		else if([ImageFileext isEqualToString:@"ps"]){
			id_format = SAVE_PS;
		}
		else {
			id_format = (int) CurrentImageFormat;
			ImageFilehead = [[ImageSavePanelObj URL] path];
		};
		
		[_kemoviewer SaveGLBufferToFileNoStep:id_format:ImageFilehead];
		[_kemoviewer UpdateImage];
	};
}

- (IBAction)ChooseImageFormatAction:(id)sender;
{
	NSUserDefaults* defaults = [_user_defaults_controller defaults];
	ImageFormatFlag = [[defaults stringForKey:@"ImageFormatID"] intValue];
	ImageFormatFlag = [[_ImageFormat_item selectedCell] tag];

}

- (IBAction)getPickSurfCommandName:(id)sender
{
	NSUserDefaults* defaults = [_user_defaults_controller defaults];
	PickSurfaceCommand = [defaults stringForKey:@"PickSurfaceCommand"];
	char command[LENGTHBUF];
	sprintf(command,"%s",[PickSurfaceCommand UTF8String]);
	set_to_pick_surface_command(command);
}

@end

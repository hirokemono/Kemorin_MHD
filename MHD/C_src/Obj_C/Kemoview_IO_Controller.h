//
//  Kemoview_IO_Controller.h
//  Open_three_windows_kemo_3
//
//  Created by Hiroaki Matsui on 10/09/20.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "DomainTableController.h"
#import "PsfController.h"
#import "FlineController.h"
#import "KemoviewerImageMaker.h"
#import "KemoviewerMovieMaker.h"
#import "KemoViewerObject.h"

#include "kemoviewer.h"

// ==================================

@interface Kemoview_IO_Controller : NSObject {

	IBOutlet NSUserDefaultsController* _user_defaults_controller;
    IBOutlet NSWindow*  window;
    IBOutlet KemoViewerMetalView   * _metalView;
	IBOutlet DomainTableController * _domainTableController;
	IBOutlet PsfController         * _psfController;
	IBOutlet FlineController       * _flineController;
    IBOutlet KemoviewerImageMaker  * _kemoImageMaker;
	IBOutlet KemoviewerMovieMaker  * _movieMakerController;
    IBOutlet KemoViewerObject      * _kmv;
    

	NSInteger ImageFormatFlag;
	NSInteger CurrentImageFormat;
	IBOutlet id _ImageFormat_item;

	NSString *ImageFileExt;

	NSInteger PsfNumberOfField;
	NSMutableArray *PsfNumberOfComponent;
	NSMutableArray *PsfFieldName;
	NSMutableArray *PsfMinimum;
	NSMutableArray *PsfMaximum;
}
@property NSInteger ImageFormatFlag;

- (void) OpenKemoviewerFile:(NSString*) kemoviewOpenFilename
                   kemoview:(struct kemoviewer_type *) kemo_sgl;

- (IBAction) OpenKemoviewerFileByMenu:(id)pId;
- (IBAction) SaveViewMatrixFile:(id)pId;
- (IBAction) LoadViewMatrixFile:(id)pId;
- (IBAction) SaveImageFile:(id)pId;

- (IBAction)ChooseImageFormatAction:(id)sender;
@end

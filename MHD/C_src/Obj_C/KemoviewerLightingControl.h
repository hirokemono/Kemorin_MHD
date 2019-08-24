//
//  KemoviewerLightingControl.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/08/23.
//

#ifndef KemoviewerLightingControl_h__
#define KemoviewerLightingControl_h__

#import <Cocoa/Cocoa.h>
#import "KemoViewerOpenGLView.h"
#import "fillRectView.h"


@interface LightTableController : NSObject {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet KemoViewerOpenGLView*  _kemoviewer;
	IBOutlet id _lightTableView;

	IBOutlet NSTableView * idlightTableView;

	NSInteger  numLightTable;
	NSMutableArray *radialLightPosition;
	NSMutableArray *elevationLightPosition;
	NSMutableArray *azimuthLightPosition;

	CGFloat ambientMaterial;
	CGFloat diffuseMaterial;
	CGFloat specularMaterial;
	CGFloat shinessMaterial;
}
@property (assign) NSMutableArray * radialLightPosition;
@property (assign) NSMutableArray * elevationLightPosition;
@property (assign) NSMutableArray * azimuthLightPosition;
@property (assign) NSTableView * idlightTableView;

@property NSInteger numLightTable;

@property CGFloat ambientMaterial;
@property CGFloat diffuseMaterial;
@property CGFloat specularMaterial;
@property CGFloat shinessMaterial;

- (void)awakeFromNib;


- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;

- (void)InitLightTable;
- (void)SetLightTable;

- (IBAction)UpdateLightTable:(id)pID;

- (IBAction)SetAmbientMaterialAction:(id)sender;
- (IBAction)SetDiffuseMaterialAction:(id)sender;
- (IBAction)SetSpecularMaterialAction:(id)sender;
- (IBAction)SetShinenessMaterialAction:(id)sender;

@end

#endif /* KemoviewerLightingControl_h */

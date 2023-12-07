//
//  KemoviewerLightingControl.h
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/08/23.
//

#ifndef KemoviewerLightingControl_h__
#define KemoviewerLightingControl_h__

@import Cocoa;

#import "KemoViewerMetalView.h"
#import "fillRectView.h"
#import "KemoViewerObject.h"
#include "Kemoviewer.h"


@interface LightTableController : NSObject {
	IBOutlet NSUserDefaultsController* _kemoviewGL_defaults_controller;
	IBOutlet KemoViewerMetalView*  _metalView;
    IBOutlet KemoViewerObject *_kmv;

    IBOutlet id _lightTableView;

	IBOutlet NSTableView * idlightTableView;

	IBOutlet NSSlider * radialPositionSlider;
	IBOutlet NSSlider * elevarionPositionSlider;
	IBOutlet NSSlider * azimuthPositionSlider;

	NSInteger  numLightTable;
	NSMutableArray *radialLightPosition;
	NSMutableArray *elevationLightPosition;
	NSMutableArray *azimuthLightPosition;

	CGFloat ambientMaterial;
	CGFloat diffuseMaterial;
	CGFloat specularMaterial;
	CGFloat shinessMaterial;

	CGFloat radialSliderValue;
	CGFloat elevationSliderValue;
	CGFloat azimuthSliderValue;
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

@property CGFloat radialSliderValue;
@property CGFloat elevationSliderValue;
@property CGFloat azimuthSliderValue;

- (void)awakeFromNib;


- (IBAction)addAtSelectedRow:(id)pId;
- (IBAction)deleteSelectedRow:(id)pId;

- (NSInteger)numberOfRowsInTableView:(NSTableView *)pTableViewObj;

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex;
- (void) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender;

- (void)InitLightTable;
- (void)SetLightTable:(struct kemoviewer_type *) kemo_sgl;

- (IBAction)UpdateLightTable:(id)pID;

- (IBAction)SetAmbientMaterialAction:(id)sender;
- (IBAction)SetDiffuseMaterialAction:(id)sender;
- (IBAction)SetSpecularMaterialAction:(id)sender;
- (IBAction)SetShinenessMaterialAction:(id)sender;

- (IBAction)SetRadialLightPositionAction:(id)sender;
- (IBAction)SetelevationLightPositionAction:(id)sender;
- (IBAction)SetAzimuthLightPositionAction:(id)sender;

@end

#endif /* KemoviewerLightingControl_h */

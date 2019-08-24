//
//  KemoviewerLightingControl.m
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 2019/08/23.
//

#import "KemoviewerLightingControl.h"

@implementation LightTableController
@synthesize radialLightPosition;
@synthesize elevationLightPosition;
@synthesize azimuthLightPosition;
@synthesize idlightTableView;

@synthesize numLightTable;

@synthesize ambientMaterial;
@synthesize diffuseMaterial;
@synthesize specularMaterial;
@synthesize shinessMaterial;
- (void)awakeFromNib{
	self.radialLightPosition = [[NSMutableArray alloc]init];
	self.elevationLightPosition = [[NSMutableArray alloc]init];
	self.azimuthLightPosition = [[NSMutableArray alloc]init];
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	self.numLightTable =  [[defaults stringForKey:@"numberOfLights"] intValue];
//	self.numLightTable = 0;

	self.radialLightPosition =    [[defaults arrayForKey:@"lightsRadialPosition"] mutableCopy];
	self.elevationLightPosition = [[defaults arrayForKey:@"lightsElevationPosition"] mutableCopy];
	self.azimuthLightPosition =   [[defaults arrayForKey:@"lightsAzimuthalPosition"] mutableCopy];

	self.ambientMaterial =  [[defaults stringForKey:@"materialAmbient"] floatValue];
	self.diffuseMaterial =  [[defaults stringForKey:@"materialDiffuse"] floatValue];
	self.specularMaterial = [[defaults stringForKey:@"materialSpecular"] floatValue];
	self.shinessMaterial =  [[defaults stringForKey:@"materialShineness"] floatValue];

	kemoview_alloc_phong_light_list(self.numLightTable);
	kemovier_set_material_ambient(self.ambientMaterial);
	kemoview_set_material_diffuse(self.diffuseMaterial);
	kemoview_set_material_specular(self.specularMaterial);
	kemoview_set_material_shineness(self.shinessMaterial);
	
	[self InitLightTable];
	return;
};


- (IBAction)addAtSelectedRow:(id)pId{
	float r2, t2, p2;
	int isel = [idlightTableView selectedRow];
	
	if ([idlightTableView selectedRow] >= 0) {
		r2 = [[self.radialLightPosition    objectAtIndex:isel  ] floatValue];
		t2 = [[self.elevationLightPosition objectAtIndex:isel  ] floatValue];
 		p2 = [[self.azimuthLightPosition   objectAtIndex:isel  ] floatValue];
		kemoview_add_phong_light_list(isel, r2, t2, p2);
		
		[self SetLightTable];
	}
	[_kemoviewer UpdateImage];
	return;
};
- (IBAction)deleteSelectedRow:(id)pId{
	int i, n;
	
	NSIndexSet *SelectedList = [idlightTableView selectedRowIndexes];
	n = [self.radialLightPosition count];
	if(n < 1) return;
	
	if ([idlightTableView numberOfSelectedRows] > 0) {
		for(i= n;i>0;i--){
			if([SelectedList containsIndex:i] == TRUE){
				kemoview_delete_phong_light_list(i);
			}
		};
	}
	
	[self SetLightTable];
	[_kemoviewer UpdateImage];
	return;
};

- (int)numberOfRowsInTableView:(NSTableView *)pTableViewObj{
	return [self.radialLightPosition count];
};

- (id) tableView:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex{
	if ([[pTableColumn identifier] isEqualToString:@"Radius"]) {
		return [self.radialLightPosition objectAtIndex:pRowIndex];
	}
	if ([[pTableColumn identifier] isEqualToString:@"Elevation"]) {
		return [self.elevationLightPosition objectAtIndex:pRowIndex];
	}
	if ([[pTableColumn identifier] isEqualToString:@"Azimuth"]) {
		return [self.azimuthLightPosition objectAtIndex:pRowIndex];
	}
	
	NSLog(@"***ERROR** dropped through pTableColumn identifiers");
	return NULL;
};

- (void)tableView:(NSTableView *)pTableViewObj setObjectValue:(id)pObject 
   forTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex{
	float r0, t0, p0;
	float r_in, r1, r2;
	
	int numberOfRaw = [self.radialLightPosition count];
	
	r0 =  [[self.radialLightPosition objectAtIndex:pRowIndex] floatValue];
	t0 =  [[self.elevationLightPosition objectAtIndex:pRowIndex] floatValue];
	p0 =  [[self.azimuthLightPosition objectAtIndex:pRowIndex] floatValue];
	if(pRowIndex > 0){
		r1 =   [[self.radialLightPosition objectAtIndex:(pRowIndex-1)] floatValue];
	}
	if(pRowIndex < numberOfRaw-1){
		r2 =   [[self.radialLightPosition objectAtIndex:(pRowIndex+1)] floatValue];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Radius"]) {
		r_in = [(NSString *)pObject floatValue];
		if(numberOfRaw == 1){
			r0 = r_in;
		} else if(pRowIndex > 0 && r_in < r1){
			r0 = r1;
		} else if(pRowIndex < (numberOfRaw-1) && r_in > r2){
			r0 = r2;
		} else {
			r0 = r_in;
		};
		
		[self.radialLightPosition replaceObjectAtIndex:pRowIndex
										withObject:[[NSNumber alloc] initWithFloat:r0]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Elevation"]) {
		t0 = [(NSString *)pObject floatValue];
		[self.elevationLightPosition replaceObjectAtIndex:pRowIndex
										withObject:[[NSNumber alloc] initWithFloat:t0]];
	}
	
	if ([[pTableColumn identifier] isEqualToString:@"Azimuth"]) {
		p0 = [(NSString *)pObject floatValue];
		[self.azimuthLightPosition replaceObjectAtIndex:pRowIndex
											   withObject:[[NSNumber alloc] initWithFloat:p0]];
	}
	
	kemoview_set_each_light_position(pRowIndex, r0, t0, p0);
	
	[_kemoviewer UpdateImage];
	[self SetLightTable];
} // end tableView:setObjectValue:forTableColumn:row:


- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}



- (void)InitLightTable{
	int i, n_ini;
	float r, t, p;
	
	n_ini = kemoview_get_num_light_position();
	for(i=0;i<self.numLightTable;i++){
		r =  [[self.radialLightPosition objectAtIndex:i] floatValue];
		t =  [[self.elevationLightPosition objectAtIndex:i] floatValue];
		p =  [[self.azimuthLightPosition objectAtIndex:i] floatValue];
		if(i < n_ini){
			kemoview_set_each_light_position(i, r, t, p);
		} else {
			kemoview_add_phong_light_list(i, r, t, p);
		};
	};
	for(i=n_ini; i>self.numLightTable;i--){
		kemoview_delete_phong_light_list(i-1);
	};
	[self SetLightTable];
	return;
};
- (void)SetLightTable{
	int i;
	float r, t, p;
	//	double value, color, color;
	
	[radialLightPosition removeAllObjects];
	[elevationLightPosition removeAllObjects];
	[azimuthLightPosition removeAllObjects];
	self.numLightTable = kemoview_get_num_light_position();
	for(i=0;i<self.numLightTable;i++){
		kemoview_get_each_light_rtp(i, &r, &t, &p);
		[self.radialLightPosition addObject:[[NSNumber alloc ] initWithFloat:r] ];
		[self.elevationLightPosition addObject:[[NSNumber alloc ] initWithFloat:t] ];
		[self.azimuthLightPosition addObject:[[NSNumber alloc ] initWithFloat:p] ];
	}
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setInteger:((int) self.numLightTable) forKey:@"numberOfLights"];

	[defaults setObject:self.radialLightPosition forKey:@"lightsRadialPosition"];
	[defaults setObject:self.elevationLightPosition forKey:@"lightsElevationPosition"];
	[defaults setObject:self.azimuthLightPosition forKey:@"lightsAzimuthalPosition"];

	[_lightTableView reloadData];
	return;
};

- (IBAction)UpdateLightTable:(id)pID{
	[self SetLightTable];
	return;
};


- (IBAction)SetAmbientMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.diffuseMaterial) forKey:@"materialAmbient"];
	kemovier_set_material_ambient(self.ambientMaterial);
	[_kemoviewer UpdateImage];
	return;
};
- (IBAction)SetDiffuseMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.diffuseMaterial) forKey:@"materialDiffuse"];
	kemoview_set_material_diffuse(self.diffuseMaterial);
	[_kemoviewer UpdateImage];
	return;
};
- (IBAction)SetSpecularMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.specularMaterial) forKey:@"materialSpecular"];
	kemoview_set_material_specular(self.specularMaterial);
	[_kemoviewer UpdateImage];
	return;
};
- (IBAction)SetShinenessMaterialAction:(id)sender{
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	[defaults setFloat:((float) self.shinessMaterial) forKey:@"materialShineness"];
	kemoview_set_material_shineness(self.shinessMaterial);
	[_kemoviewer UpdateImage];
	return;
};

@end

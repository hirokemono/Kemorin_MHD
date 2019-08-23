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

@synthesize ambientMaterial;
@synthesize diffuseMaterial;
@synthesize specularMaterial;
@synthesize shinessMaterial;
- (void)awakeFromNib{
	float r, t, p;
	
	self.radialLightPosition = [[NSMutableArray alloc]init];
	self.elevationLightPosition = [[NSMutableArray alloc]init];
	self.azimuthLightPosition = [[NSMutableArray alloc]init];
	
	NSUserDefaults* defaults = [_kemoviewGL_defaults_controller defaults];
	r = [[defaults stringForKey:@"lightRadial"] floatValue];
	t = [[defaults stringForKey:@"lightTheta"] floatValue];
	p = [[defaults stringForKey:@"lightPhi"] floatValue];
	
	self.ambientMaterial =  [[defaults stringForKey:@"materialAmbient"] floatValue];
	self.diffuseMaterial =  [[defaults stringForKey:@"materialDiffuse"] floatValue];
	self.specularMaterial = [[defaults stringForKey:@"materialSpecular"] floatValue];
	self.shinessMaterial =  [[defaults stringForKey:@"materialShineness"] floatValue];
	
	kemovier_set_material_ambient(self.ambientMaterial);
	kemoview_set_material_diffuse(self.diffuseMaterial);
	kemoview_set_material_specular(self.specularMaterial);
	kemoview_set_material_shineness(self.shinessMaterial);
	return;
};


- (IBAction)addAtSelectedRow:(id)pId{
	float r0, t0, p0;
	float r1, t1, p1;
	float r2, t2, p2;
	int isel = [idlightTableView selectedRow];
	
	if ([idlightTableView selectedRow] > 0 && isel > 0) {
		r1 = [[self.radialLightPosition    objectAtIndex:isel-1] floatValue];
		t1 = [[self.elevationLightPosition objectAtIndex:isel-1] floatValue];
		p1 = [[self.azimuthLightPosition   objectAtIndex:isel-1] floatValue];
		r2 = [[self.radialLightPosition    objectAtIndex:isel  ] floatValue];
		t2 = [[self.elevationLightPosition objectAtIndex:isel  ] floatValue];
 		p2 = [[self.azimuthLightPosition   objectAtIndex:isel  ] floatValue];
		r0 = (r1 + r2)*HALF;
		t0 = (t1 + t2)*HALF;
		p0 = (p1 + p2)*HALF;
		kemoview_add_phong_light_list(r0, t0, p0);
		
		[self SetLightTable];
	}
	[_kemoviewer UpdateImage];
	return;
};
- (IBAction)deleteSelectedRow:(id)pId{
	int i;
	
	NSIndexSet *SelectedList = [idlightTableView selectedRowIndexes];
	if([self.radialLightPosition count] < 2) return;
	
	if ([idlightTableView numberOfSelectedRows] > 0) {
		for(i = [self.radialLightPosition count]-1;i>1;i--){
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
	if ([[pTableColumn identifier] isEqualToString:@"R"]) {
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

- (IBAction) ViewSelection:(NSTableView *)pTableViewObj objectValueForTableColumn:(NSTableColumn *)pTableColumn row:(int)pRowIndex :(id)sender{
	NSLog(@"Selected Column and raws id:   %@ %d",[pTableColumn identifier],pRowIndex);
}



- (void)InitLightTable{
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
	numLightTable = kemoview_get_num_light_position();
	for(i=0;i<numLightTable;i++){
		kemoview_get_each_light_rtp(i, &r, &t, &p);
		[radialLightPosition addObject:[[NSNumber alloc ] initWithFloat:r] ];
		[elevationLightPosition addObject:[[NSNumber alloc ] initWithFloat:t] ];
		[azimuthLightPosition addObject:[[NSNumber alloc ] initWithFloat:p] ];
	}
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

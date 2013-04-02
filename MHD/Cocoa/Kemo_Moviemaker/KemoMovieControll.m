//
//  KemoMovieControll.m
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 10/10/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import "KemoMovieControll.h"


@implementation KemoMovieControll
@synthesize evolutionCurrentStep;
@synthesize evolutionStartStep;
@synthesize evolutionEndStep;
@synthesize evolutionIncrement;
@synthesize evolutionFPS;
- (id)init;
{
	self.evolutionCurrentStep = 1;
	self.evolutionStartStep = 1;
	self.evolutionEndStep =   1;
	self.evolutionIncrement = 1;
	self.evolutionFPS = 12;
	return self;
}	

-(void) ImageToQTMovie{
	NSImage *anImage;
	NSDictionary *movieDict = nil;
	QTTime duration = QTMakeTime(1, self.evolutionFPS);
	
	// when adding images we must provide a dictionary
	// specifying the codec attributes
	movieDict = [NSDictionary dictionaryWithObjectsAndKeys:@"mp4v",
				 QTAddImageCodecType,
				 [NSNumber numberWithLong:codecMaxQuality],
				 QTAddImageCodecQuality,
				 nil];

	anImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
	// [anImage setFlipped:YES];
	// [anImage lockFocusOnRepresentation:bmpRep]; // This will flip the rep.
	// [anImage unlockFocus];
	
	// Adds an image for the specified duration to the QTMovie
	[mMovie addImage:anImage forDuration:duration withAttributes:movieDict];
	[mMovie updateMovieFile];
	// free up our image object
	/* deallocate memory*/
	[anImage release];
	return;
}


-(IBAction) SaveImageEvolution:(id)pSender
{
	//	NSLog(@"SaveRotation received message = %@",(NSString*)[pNotification object]);
	NSError *overWriteflag = [[NSError alloc] init];
	
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose one of image files"];
	NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModalForTypes:nil];
	if(PsfOpenInteger == NSOKButton){
		imageFileName =  [PsfOpenPanelObj filename];
		imageFileExt =   [imageFileName pathExtension];
		imageFileHead =  [imageFileName stringByDeletingPathExtension];
		imageFileHeadExStep =  [imageFileHead stringByDeletingPathExtension];
		// NSLog(@"PSF file name =      %@",PsfOpenFilename);
		// NSLog(@"PSF file header =    %@",PsfOpenFilehead);
	}
	else { return;};
	
	NSSavePanel *evolutionImageSavePanelObj	= [NSSavePanel savePanel];
	NSInteger EvolutionSaveInteger	= [evolutionImageSavePanelObj runModalForTypes:nil];
	[evolutionImageSavePanelObj setCanSelectHiddenExtension:YES];	

	if(EvolutionSaveInteger == NSOKButton){
		movieFileName = [evolutionImageSavePanelObj filename];
		movieFileHead = [movieFileName stringByDeletingPathExtension];
		movieFileName = [movieFileHead stringByAppendingPathExtension:@"mov"];
		NSError *overWriteflag = [[NSError alloc] init];
	}
	else { return;};
	
		// Create a QTMovie with a writable data reference
	NSLog(@"EvolutionImageFileName: %@", movieFileName);
	mMovie = [[QTMovie alloc] initToWritableFile:movieFileName error:&overWriteflag];
	
	if(overWriteflag!= NULL ){
		NSFileManager *fman = [NSFileManager defaultManager];
		[fman removeFileAtPath:movieFileName handler: nil ];
		mMovie = [[QTMovie alloc] initToWritableFile:movieFileName error:NULL];
	}
	// mark the movie as editable
	[mMovie setAttribute:[NSNumber numberWithBool:YES] forKey:QTMovieFlatten];

	[progreessBar setIndeterminate:NO];
	[progreessBar startAnimation:(id)pSender];
	for (self.evolutionCurrentStep = self.evolutionStartStep; 
		 self.evolutionCurrentStep<(self.evolutionEndStep+1);
		 self.evolutionCurrentStep++) {
		if( ((self.evolutionCurrentStep-self.evolutionStartStep)%self.evolutionIncrement) == 0) {
			imageFileHead =  [imageFileHeadExStep stringByAppendingPathExtension:
							[NSString stringWithFormat:@"%d",self.evolutionCurrentStep]];
			imageFileName =  [imageFileHead stringByAppendingPathExtension:imageFileExt];
			[self ImageToQTMovie];
			[progreessBar incrementBy:(double)self.evolutionIncrement];
			[progreessBar displayIfNeeded];
		};
	};
	[progreessBar setDoubleValue:(double)self.evolutionStartStep];
	[progreessBar displayIfNeeded];
	[mMovie release];
	return;
}

- (IBAction)SetEvolutionSteps:(id)pSender{
	NSLog(@"start: %d", self.evolutionStartStep);
	NSLog(@"end: %d", self.evolutionEndStep);
	NSLog(@"increment: %d", self.evolutionIncrement);
	NSLog(@"FPS: %d", self.evolutionFPS);
}

@end

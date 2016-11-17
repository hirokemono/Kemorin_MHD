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

-(void) OpenKemoviewMovieFile:(NSString *)mFileName
{
    NSError *overWriteflag = [[NSError alloc] init];
    // Create a QTMovie with a writable data reference
    NSLog(@"EvolutionImageFileName: %@", mFileName);
    mMovie = [[QTMovie alloc] initToWritableFile:mFileName error:&overWriteflag];
    
    if(overWriteflag!= NULL ){
        NSFileManager *fman = [NSFileManager defaultManager];
        [fman removeFileAtPath:mFileName handler: nil ];
        mMovie = [[QTMovie alloc] initToWritableFile:mFileName error:NULL];
    }

    // mark the movie as editable
    [mMovie setAttribute:[NSNumber numberWithBool:YES] forKey:QTMovieFlatten];
    duration = QTMakeTime(1, self.evolutionFPS);
}

-(void) CloseKemoviewMovieFile{
    [mMovie release];
}

-(void) ImageToMovie{
    // when adding images we must provide a dictionary
    // specifying the codec attributes
    NSDictionary *movieDict = nil;
    movieDict = [NSDictionary dictionaryWithObjectsAndKeys:@"mp4v",
                 QTAddImageCodecType,
                 [NSNumber numberWithLong:codecMaxQuality],
                 QTAddImageCodecQuality,
                 nil];
    
    SnapshotImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
    // [SnapshotImage setFlipped:YES];
    // [SnapshotImage lockFocusOnRepresentation:bmpRep]; // This will flip the rep.
    // [SnapshotImage unlockFocus];
    
    // Adds an image for the specified duration to the QTMovie
    [mMovie addImage:SnapshotImage forDuration:duration withAttributes:movieDict];
    [mMovie updateMovieFile];
    // free up our image object
    /* deallocate memory*/
    [SnapshotImage release];
    return;
}

-(void) OpenQTMovieFile:(NSString *)mFileName
{
    NSError *overWriteflag = [[NSError alloc] init];
    // Create a QTMovie with a writable data reference
    NSLog(@"EvolutionImageFileName: %@", mFileName);
    mMovie = [[QTMovie alloc] initToWritableFile:mFileName error:&overWriteflag];
    
    if(overWriteflag!= NULL ){
        NSFileManager *fman = [NSFileManager defaultManager];
        [fman removeFileAtPath:mFileName handler: nil ];
        mMovie = [[QTMovie alloc] initToWritableFile:mFileName error:NULL];
    }
    
    // mark the movie as editable
    [mMovie setAttribute:[NSNumber numberWithBool:YES] forKey:QTMovieFlatten];
    duration = QTMakeTime(1, self.evolutionFPS);
}

-(void) CloseQTMovieFile{
    [mMovie release];
}

-(void) ImageToQTMovie{
    // when adding images we must provide a dictionary
    // specifying the codec attributes
    NSDictionary *movieDict = nil;
    movieDict = [NSDictionary dictionaryWithObjectsAndKeys:@"mp4v",
                 QTAddImageCodecType,
                 [NSNumber numberWithLong:codecMaxQuality],
                 QTAddImageCodecQuality,
                 nil];
    
    SnapshotImage = [[NSImage alloc] initWithContentsOfFile:imageFileName];
    // [SnapshotImage setFlipped:YES];
    // [SnapshotImage lockFocusOnRepresentation:bmpRep]; // This will flip the rep.
    // [SnapshotImage unlockFocus];
    
    // Adds an image for the specified duration to the QTMovie
    [mMovie addImage:SnapshotImage forDuration:duration withAttributes:movieDict];
    [mMovie updateMovieFile];
    // free up our image object
    /* deallocate memory*/
    [SnapshotImage release];
    return;
}


-(IBAction) SaveImageEvolution:(id)pSender
{
    int i, ist, ied, inc;
	//	NSLog(@"SaveRotation received message = %@",(NSString*)[pNotification object]);
	
	NSOpenPanel *PsfOpenPanelObj	= [NSOpenPanel openPanel];
	[PsfOpenPanelObj setTitle:@"Choose one of image files"];
	NSInteger PsfOpenInteger	= [PsfOpenPanelObj runModal];
	if(PsfOpenInteger == NSFileHandlingPanelOKButton){
		imageFileName =  [[PsfOpenPanelObj URL] path];
		imageFileExt =   [imageFileName pathExtension];
		imageFileHead =  [imageFileName stringByDeletingPathExtension];
		imageFileHeadExStep =  [imageFileHead stringByDeletingPathExtension];
		// NSLog(@"PSF file name =      %@",PsfOpenFilename);
		// NSLog(@"PSF file header =    %@",PsfOpenFilehead);
	}
	else { return;};
	
	NSSavePanel *evolutionImageSavePanelObj	= [NSSavePanel savePanel];
	NSInteger EvolutionSaveInteger	= [evolutionImageSavePanelObj runModal];
	[evolutionImageSavePanelObj setCanSelectHiddenExtension:YES];	

	if(EvolutionSaveInteger == NSFileHandlingPanelOKButton){
		movieFileName = [[evolutionImageSavePanelObj URL] path];
		movieFileHead = [movieFileName stringByDeletingPathExtension];
		movieFileName = [movieFileHead stringByAppendingPathExtension:@"mov"];
        oldmovieFileName = [movieFileName stringByAppendingPathExtension:@"mov"];
	}
	else { return;};
	
    ist = self.evolutionStartStep;
    ied = self.evolutionEndStep;
    inc = self.evolutionIncrement;
    
    [self OpenQTMovieFile:oldmovieFileName];
	for (i = ist; i<(ied+1); i++) {
        self.evolutionCurrentStep = i;
		if( ((i-ist)%inc) == 0) {
			imageFileHead =  [imageFileHeadExStep stringByAppendingPathExtension:
							[NSString stringWithFormat:@"%d",i]];
			imageFileName =  [imageFileHead stringByAppendingPathExtension:imageFileExt];
			[self ImageToQTMovie];
		};
	};
    [self CloseQTMovieFile];
    
    [progreessBar setIndeterminate:NO];
    [progreessBar startAnimation:(id)pSender];
    [self OpenKemoviewMovieFile:movieFileName];
    
    for (i = ist;i<(ied+1);i++) {
        self.evolutionCurrentStep = i;
        if( ((i-ist)%inc) == 0) {
            imageFileHead =  [imageFileHeadExStep stringByAppendingPathExtension:
                              [NSString stringWithFormat:@"%d",i]];
            imageFileName =  [imageFileHead stringByAppendingPathExtension:imageFileExt];
            [self ImageToMovie];
            [progreessBar incrementBy:(double)inc];
            [progreessBar displayIfNeeded];
        };
    };
    [self CloseKemoviewMovieFile];
    [progreessBar setDoubleValue:(double)ist];
    [progreessBar displayIfNeeded];
	return;
}

- (IBAction)SetEvolutionSteps:(id)pSender{
	NSLog(@"start: %d", (int) self.evolutionStartStep);
	NSLog(@"end: %d", (int) self.evolutionEndStep);
	NSLog(@"increment: %d", (int) self.evolutionIncrement);
	NSLog(@"FPS: %d", (int) self.evolutionFPS);
}

@end

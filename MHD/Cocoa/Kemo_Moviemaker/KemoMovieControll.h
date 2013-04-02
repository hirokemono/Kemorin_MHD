//
//  KemoMovieControll.h
//  Kemo_Moviemaker
//
//  Created by Hiroaki Matsui on 10/10/26.
//  Copyright 2010 Department of Geophysical Sciences, University of Chicago. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <QTKit/QTKit.h>


@interface KemoMovieControll : NSObject {

	NSInteger evolutionCurrentStep;
	NSInteger evolutionStartStep;
	NSInteger evolutionEndStep;
	NSInteger evolutionIncrement;
	NSInteger evolutionFPS;
	
	QTMovie *mMovie;
	IBOutlet QTMovieView *movieView;
	IBOutlet NSProgressIndicator *progreessBar;
	NSString *movieFileName;
	NSString *movieFileHead;
	NSString *movieFileExt;
	
	NSString *imageFileName;
	NSString *imageFileHead;
	NSString *imageFileExt;
	NSString *imageFileHeadExStep;
	
}
@property NSInteger evolutionCurrentStep;
@property NSInteger evolutionStartStep;
@property NSInteger evolutionEndStep;
@property NSInteger evolutionIncrement;
@property NSInteger evolutionFPS;

-(void) ImageToQTMovie;
-(IBAction) SaveImageEvolution:(id)pSender;
-(IBAction)SetEvolutionSteps:(id)pSender;

@end

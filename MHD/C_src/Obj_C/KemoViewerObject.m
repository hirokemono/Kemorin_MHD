/*
//  KemoViewerObject.m
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/4/23.
*/

@import Foundation;
#import "KemoViewerObject.h"

@implementation KemoViewerObject
- (id) init{
    self->single_kemoview = kemoview_allocate_single_viwewer_struct();
    return self;
}
- (struct kemoviewer_type *) KemoViewPointer
{
    return self->single_kemoview;
}
@end


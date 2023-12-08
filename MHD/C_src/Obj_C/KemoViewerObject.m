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
#ifdef __APPLE__
    printf("KemoViewerObject init Start!!\n");
#endif
    self.kemoview_metal = kemoview_allocate_single_viwewer_struct();
    reset_all_view_parameter(self.kemoview_metal->view_s);
    reset_to_init_angle(self.kemoview_metal->view_s);
    update_projection_struct(self.kemoview_metal->view_s);
    modify_view_by_struct(self.kemoview_metal->view_s);
    return self;
}
- (struct kemoviewer_type *) KemoViewPointer
{
    return self.kemoview_metal;
}
@end


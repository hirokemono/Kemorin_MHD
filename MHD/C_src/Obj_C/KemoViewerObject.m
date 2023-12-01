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
    struct kemoviewer_type * kemo_sgl = kemoview_single_viwewer_struct();
    reset_all_view_parameter(kemo_sgl->view_s);
    reset_to_init_angle(kemo_sgl->view_s);
    update_projection_struct(kemo_sgl->view_s);
    modify_view_by_struct(kemo_sgl->view_s);
    return self;
}
- (struct kemoviewer_type *) KemoViewPointer
{
    return self->single_kemoview;
}
@end


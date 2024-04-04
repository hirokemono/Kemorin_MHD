/*
//  KemoViewerObject.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/4/23.
*/

#ifndef KemoViewerObject_h
#define KemoViewerObject_h

#include "kemoviewer.h"
#include "m_kemoviewer_data.h"
#include "m_gl_transfer_matrix.h"

@interface KemoViewerObject : NSObject {
    struct kemoviewer_type *kemoview_metal;
}
@property struct kemoviewer_type *kemoview_metal;

- (id) init;
- (struct kemoviewer_type *) KemoViewPointer;

@end

#endif /* KemoViewerObject_h */

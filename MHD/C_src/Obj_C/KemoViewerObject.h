/*
//  KemoViewerObject.h
//  CalypsoView_Cocoa
//
//  Created by Hiroaki Matsui on 11/4/23.
*/

#ifndef KemoViewerObject_h
#define KemoViewerObject_h

#include "kemoviewer.h"

@interface KemoViewerObject : NSObject {
    struct kemoviewer_type *single_kemoview;
}

- (id) init;
- (struct kemoviewer_type *) KemoViewPointer;

@end

#endif /* KemoViewerObject_h */

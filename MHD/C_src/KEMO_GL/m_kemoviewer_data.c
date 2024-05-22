/*
//  m_kemoviewer_data.c
//  Kemoview_Cocoa
//
//  Created by Hiroaki Matsui on 12/08/13.
//
*/

#include "m_kemoviewer_data.h"

void set_default_image_format_id(struct kemoviewer_type *kemo_sgl, int input){
    kemo_sgl->image_format_id = input;
    return;
}
int send_default_image_format_id(struct kemoviewer_type *kemo_sgl){return kemo_sgl->image_format_id;};

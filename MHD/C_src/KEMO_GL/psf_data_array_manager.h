
/* psf_data_array_manager.h*/

#ifndef PSF_DATA_ARRAY_MANAGER_
#define PSF_DATA_ARRAY_MANAGER_

#include "m_kemoview_psf_menu.h"


/* prototypes */

void check_array_size(struct kemo_array_control *kemo_array);
int add_new_kemoview_array(struct kemo_array_control *kemo_array);
void set_close_current_kemoview_array(struct kemo_array_control *kemo_array);

#endif

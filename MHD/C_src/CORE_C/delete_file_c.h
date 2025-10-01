/*********************************************************************
    delete_file_c.h
*********************************************************************/

#ifndef DELETE_FILE_C__
#define DELETE_FILE_C__

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "calypso_param_c.h"

/* prototypes */

void delete_file_c(const char *txt_file_name);
void chdir_c(char path_name[513]);
void getcwd_c(char path_name[513]);
#endif

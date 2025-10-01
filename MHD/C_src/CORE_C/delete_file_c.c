/*********************************************************************
    delete_file_c.c
*********************************************************************/

#include <string.h>
#include "delete_file_c.h"

void delete_file_c(const char *txt_file_name)
{
	if(remove(txt_file_name) == 0){
		printf(" %s is deleted.\n", txt_file_name);
	} else {
		printf(" Delete error \n");
		exit(1);
	};
	return;
}

void chdir_c(char path_name[513]){
    chdir(path_name);
    return;
};

void getcwd_c(char path_name[513]){
    getcwd(path_name, 512);
    return;
};

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "m_field_name_from_f.h"
#include "m_force_name_from_f.h"

int main(int argc, char **argv)
{
	int i, j;
	struct field_names_f *fld_names = init_field_name_f();
    check_field_name_f(fld_names);
	dealloc_field_name_f(fld_names);
	
	struct force_names_f *frc_names = init_force_name_f();
	
	printf("nword %d %d \n", frc_names->len_f, frc_names->ntot_forcrs);
	for(i=0;i<frc_names->ntot_forcrs;i++){
		printf("name: %d: %d: %s: %s\n", i, frc_names->num_comp[i],
			   frc_names->force_name[i], frc_names->forth_math[i]);
	}
	printf("\n");
	dealloc_force_name_f(frc_names);
	
	
	struct advection_names_f *adv_names = init_advection_name_f();
	
	printf("nword %d %d \n", adv_names->len_f, adv_names->ntot_advection);
	for(i=0;i<adv_names->ntot_advection;i++){
		printf("name: %d: %d: %s: %s\n", i, adv_names->num_comp[i],
			   adv_names->term_name[i], adv_names->term_math[i]);
	}
	printf("\n");
	dealloc_advection_name_f(adv_names);
	
	return 0;
};

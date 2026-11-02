/***********************************************************************
 !!  convert_one_tensor_from_cyl.c
 !!
 !!  Created by Hiroaki Matsui on Oct. 30, 2026
 !!  Copyright 2026 Dept. of Earth and Planetary Sciences, Tokyo Tech.
 !!
 !!   uxux = (as)^2 *(x*x *usus - x*y *usup
 !!                 - x*y *usup + y*y *upup)
 !!   uxuy = (as)^2 *(x*y *usus + x*x *usup
 !!                 - y*y *usup - y*x *upup)
 !!   uxuz = (as)^2 *(x*s *usuz - y*s *upuz)
 !!   uyuy = (as)^2 *(y*y *usus + x*y *usup
 !!                 + x*y *usup + x*x *upup)
 !!   uyuz = (as)^2 *(y*s *usuz + x*s *upuz)
 !!   uzuz = (as)^2 * s*s* uzuz
 ***********************************************************************/
#include "convert_one_tensor_from_cyl.h"

#include "convert_one_tensor_from_cyl.frag"

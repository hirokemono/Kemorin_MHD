!gravity_vec_each_ele_1st.f90
!      module gravity_vec_each_ele_1st
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on May, 2009
!
!      subroutine set_gravity_vec_each_ele_1st(k2, i_field,             &
!     &          ak_buo, vect_e)
!      subroutine set_double_gvec_each_ele_1st(k2, i_src1, i_src2,      &
!     &          ak_buo1, ak_buo2, vect_e)
!
!      subroutine set_gravity_on_each_ele_1st(k2, nd, i_field,          &
!     &          ak_buo, buo_e)
!      subroutine set_double_g_each_ele_1st(k2, nd, i_src1, i_src2,     &
!     &         ak_buo1, ak_buo2, buo_e)
!
      module gravity_vec_each_ele_1st
!
      use m_precision
      use m_machine_parameter
      use m_geometry_data
      use m_physical_property
      use m_node_phys_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_gravity_vec_each_ele_1st(k2, i_field,              &
     &          ak_buo, vect_e)
!
      use gravity_vec_on_each_element
!
      integer(kind = kint), intent(in) :: k2, i_field
      real (kind=kreal), intent(in) :: ak_buo(ele1%numele)
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
!
      if (i_grav .eq. iflag_const_g) then
        call const_gvec_each_element(node1%numnod, ele1%numele,         &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      k2, i_field, num_tot_nod_phys, d_nod, grav, ak_buo, vect_e)
      else if (i_grav .eq. iflag_radial_g) then
        call radial_gvec_each_element(node1%numnod, ele1%numele,        &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, node1%a_r, k2, i_field, num_tot_nod_phys,         &
     &      d_nod, ak_buo, vect_e)
      else if (i_grav .eq. iflag_self_r_g) then
        call self_gvec_each_element(node1%numnod, ele1%numele,          &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, k2, i_field, num_tot_nod_phys,                    &
     &      d_nod, ak_buo, vect_e)
      end if
!
      end subroutine set_gravity_vec_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine set_double_gvec_each_ele_1st(k2, i_src1, i_src2,       &
     &         ak_buo1, ak_buo2, vect_e)
!
      use gravity_vec_on_each_element
!
      integer(kind = kint), intent(in) :: k2, i_src1, i_src2
      real(kind = kreal), intent(in) :: ak_buo1(ele1%numele)
      real(kind = kreal), intent(in) ::  ak_buo2(ele1%numele)
      real(kind  =kreal), intent(inout) :: vect_e(ele1%numele,3)
!
!
      if (i_grav .eq. iflag_const_g) then
        call const_double_gvec_each_element(node1%numnod, ele1%numele,  &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      k2, i_src1, i_src2, num_tot_nod_phys, d_nod,                &
     &      grav, ak_buo1, ak_buo2, vect_e)
      else if (i_grav .eq. iflag_radial_g) then
        call radial_double_gvec_each_element(node1%numnod, ele1%numele, &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, node1%a_r, k2, i_src1, i_src2, num_tot_nod_phys,  &
     &      d_nod, ak_buo1, ak_buo2, vect_e)
      else if (i_grav .eq. iflag_self_r_g) then
        call self_double_gvec_each_element(node1%numnod, ele1%numele,   &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, k2, i_src1, i_src2, num_tot_nod_phys, d_nod,      &
     &      ak_buo1, ak_buo2, vect_e)
      end if
!
      end subroutine set_double_gvec_each_ele_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_gravity_on_each_ele_1st(k2, nd, i_field,           &
     &          ak_buo, buo_e)
!
      use gravity_scl_on_each_element
!
      integer(kind = kint), intent(in) :: nd, k2, i_field
      real (kind=kreal), intent(in) :: ak_buo(ele1%numele)
      real (kind=kreal), intent(inout) :: buo_e(ele1%numele)
!
!
      if (i_grav .eq. iflag_const_g) then
        call const_g_each_element(node1%numnod, ele1%numele,            &
     &      ele1%nnod_4_ele,  ele1%ie, np_smp, ele1%istack_ele_smp,     &
     &      nd, k2, i_field, num_tot_nod_phys, d_nod,                   &
     &      grav, ak_buo, buo_e)
      else if (i_grav .eq. iflag_radial_g) then
        call radial_g_each_element(node1%numnod, ele1%numele,           &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, node1%a_r, nd, k2, i_field, num_tot_nod_phys,     &
     &      d_nod, ak_buo, buo_e)
      else if (i_grav .eq. iflag_self_r_g) then
        call self_g_each_element(node1%numnod, ele1%numele,             &
     &      ele1%nnod_4_ele, ele1%ie, np_smp, ele1%istack_ele_smp,      &
     &      node1%xx, nd, k2, i_field, num_tot_nod_phys,                &
     &      d_nod, ak_buo, buo_e)
      end if
!
      end subroutine set_gravity_on_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      subroutine set_double_g_each_ele_1st(k2, nd, i_src1, i_src2,      &
     &         ak_buo1, ak_buo2, buo_e)
!
      use gravity_scl_on_each_element
!
      integer(kind = kint), intent(in) :: nd, k2, i_src1, i_src2
      real(kind = kreal), intent(in) :: ak_buo1(ele1%numele)
      real(kind = kreal), intent(in) :: ak_buo2(ele1%numele)
      real(kind = kreal), intent(inout) :: buo_e(ele1%numele)
!
!
      if (i_grav .eq. iflag_const_g) then
        call const_double_g_each_element                                &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      np_smp, ele1%istack_ele_smp, nd, k2,                        &
     &      i_src1, i_src2, num_tot_nod_phys, d_nod, grav,              &
     &      ak_buo1, ak_buo2, buo_e)
      else if (i_grav .eq. iflag_radial_g) then
        call radial_double_g_each_element                               &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      np_smp, ele1%istack_ele_smp, node1%xx, node1%a_r, nd, k2,   &
     &      i_src1, i_src2, num_tot_nod_phys, d_nod, ak_buo1, ak_buo2,  &
     &      buo_e)
      else if (i_grav .eq. iflag_self_r_g) then
        call self_double_g_each_element                                 &
     &     (node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,        &
     &      np_smp, ele1%istack_ele_smp, node1%xx, nd, k2,              &
     &      i_src1, i_src2, num_tot_nod_phys, d_nod,                    &
     &      ak_buo1, ak_buo2, buo_e)
      end if
!
      end subroutine set_double_g_each_ele_1st
!
!  ---------------------------------------------------------------------
!
      end module gravity_vec_each_ele_1st

!delta_phys_2_each_surface.f90
!      module delta_phys_2_each_surface
!
!      Written by H. Matsui on Sep., 2005
!
!      subroutine dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,         &
!     &          i_scalar, scalar_sf)
!        Input:  d_nod(1,i_scalar), Output:  scalar_sf
!      subroutine dlt_vect_phys_2_each_surface(sf_grp, igrp, k2,        &
!     &          i_vector, vector_sf)
!        Input:  d_nod(1,i_tensor), Output:  vector_sf
!      subroutine dlt_tsr_phys_2_each_surface(sf_grp, igrp, k2,         &
!     &          i_tensor, tensor_sf)
!        Input:  d_nod(1,i_tensor), Output:  vector_sf
!
!      subroutine dlt_scl_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
!     &          i_scalar, ak_e, scalar_sf)
!      subroutine dlt_vect_phys_2_each_surf_coef(sf_grp, igrp, k2,      &
!     &          i_vector, ak_e, vector_sf)
!      subroutine dlt_tsr_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
!     &          i_tensor, ak_e, tensor_sf)
!
!      subroutine dlt_scl_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
!     &          i_scalar, coef, scalar_sf)
!      subroutine dlt_vect_phys_2_each_surf_cst(sf_grp, igrp, k2,       &
!     &          i_vector, coef, vector_sf)
!      subroutine dlt_tsr_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
!     &          i_tensor, coef, tensor_sf)
!
      module delta_phys_2_each_surface
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data
      use m_node_phys_data
      use t_group_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine dlt_scl_phys_2_each_surface(sf_grp, igrp, k2,          &
     &          i_scalar, scalar_sf)
!
      use set_delta_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call delta_scalar_phys_2_each_sf(np_smp, node1%numnod,            &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie,                        &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    scalar_sf)
!
      end subroutine dlt_scl_phys_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine dlt_vect_phys_2_each_surface(sf_grp, igrp, k2,         &
     &          i_vector, vector_sf)
!
      use set_delta_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_vector_phys_2_each_sf(np_smp, node1%numnod,            &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie,                        &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    vector_sf)
!
      end subroutine dlt_vect_phys_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine dlt_tsr_phys_2_each_surface(sf_grp, igrp, k2,          &
     &          i_tensor, tensor_sf)
!
      use set_delta_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call delta_tensor_phys_2_each_sf(np_smp, node1%numnod,            &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie,                        &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    tensor_sf)
!
      end subroutine dlt_tsr_phys_2_each_surface
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dlt_scl_phys_2_each_surf_coef(sf_grp, igrp, k2,        &
     &          i_scalar, ak_e, scalar_sf)
!
      use set_delta_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call dlt_scl_phys_2_each_sf_w_coef                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    ak_e, scalar_sf)
!
      end subroutine dlt_scl_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
!
      subroutine dlt_vect_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
     &          i_vector, ak_e, vector_sf)
!
      use set_delta_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call dlt_vect_phys_2_each_sf_w_coef                               &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    ak_e, vector_sf)
!
      end subroutine dlt_vect_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
!
      subroutine dlt_tsr_phys_2_each_surf_coef(sf_grp, igrp, k2,        &
     &          i_tensor, ak_e, tensor_sf)
!
      use set_delta_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call dlt_tsr_phys_2_each_sf_w_coef                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    ak_e, tensor_sf)
!
      end subroutine dlt_tsr_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dlt_scl_phys_2_each_surf_cst(sf_grp, igrp, k2,         &
     &          i_scalar, coef, scalar_sf)
!
      use set_delta_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call dlt_scl_phys_2_each_sf_w_cst                                 &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    coef, scalar_sf)
!
      end subroutine dlt_scl_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      subroutine dlt_vect_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
     &          i_vector, coef, vector_sf)
!
      use set_delta_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call dlt_vect_phys_2_each_sf_w_cst                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    coef, vector_sf)
!
      end subroutine dlt_vect_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      subroutine dlt_tsr_phys_2_each_surf_cst(sf_grp, igrp, k2,         &
     &          i_tensor, coef, tensor_sf)
!
      use set_delta_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call dlt_tsr_phys_2_each_sf_w_cst                                 &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf, surf1%node_on_sf_n,      &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, nod_fld1%ntot_phys, nod_fld1%d_fld,       &
     &    coef, tensor_sf)
!
      end subroutine dlt_tsr_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      end module delta_phys_2_each_surface

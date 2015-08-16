!node_phys_2_each_surface.f90
!      module node_phys_2_each_surface
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine scalar_phys_2_each_surface(sf_grp, igrp, k2,         &
!!     &          i_scalar, scalar_sf)
!!        Input:  d_nod(1,i_scalar), Output:  scalar_sf
!!      subroutine vector_phys_2_each_surface(sf_grp, igrp, k2,         &
!!     &          i_vector, vector_sf)
!!        Input:  d_nod(1,i_tensor), Output:  vector_sf
!!      subroutine tensor_phys_2_each_surface(sf_grp, igrp, k2,         &
!!     &          i_tensor, tensor_sf)
!!        Input:  d_nod(1,i_tensor), Output:  vector_sf
!!
!!      subroutine scalar_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
!!     &          i_scalar, ak_e, scalar_sf)
!!      subroutine vector_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
!!     &          i_vector, ak_e, vector_sf)
!!      subroutine tensor_phys_2_each_surf_coef(sf_grp, igrp, k2,       &
!!     &          i_tensor, ak_e, tensor_sf)
!!
!!      subroutine scalar_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
!!     &          i_scalar, coef, scalar_sf)
!!      subroutine vector_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
!!     &          i_vector, coef, vector_sf)
!!      subroutine tensor_phys_2_each_surf_cst(sf_grp, igrp, k2,        &
!!     &          i_tensor, coef, tensor_sf)
!
      module node_phys_2_each_surface
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
      subroutine scalar_phys_2_each_surface(sf_grp, igrp, k2,           &
     &          i_scalar, scalar_sf)
!
      use set_nodal_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call scalar_phys_2_each_sf                                        &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, num_tot_nod_phys, d_nod, scalar_sf)
!
      end subroutine scalar_phys_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine vector_phys_2_each_surface(sf_grp, igrp, k2,           &
     &          i_vector, vector_sf)
!
      use set_nodal_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call vector_phys_2_each_sf                                        &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, num_tot_nod_phys, d_nod, vector_sf)
!
      end subroutine vector_phys_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_surface(sf_grp, igrp, k2,           &
     &          i_tensor, tensor_sf)
!
      use set_nodal_2_each_surface
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call vector_phys_2_each_sf                                        &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, num_tot_nod_phys, d_nod, tensor_sf)
!
      end subroutine tensor_phys_2_each_surface
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_phys_2_each_surf_coef(sf_grp, igrp, k2,         &
     &          i_scalar, ak_e, scalar_sf)
!
      use set_nodal_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call scalar_phys_2_each_sf_w_coef                                 &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, num_tot_nod_phys, d_nod, ak_e, scalar_sf)
!
      end subroutine scalar_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
!
      subroutine vector_phys_2_each_surf_coef(sf_grp, igrp, k2,         &
     &          i_vector, ak_e, vector_sf)
!
      use set_nodal_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call vector_phys_2_each_sf_w_coef                                 &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, num_tot_nod_phys, d_nod, ak_e, vector_sf)
!
      end subroutine vector_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_surf_coef(sf_grp, igrp, k2,         &
     &          i_tensor, ak_e, tensor_sf)
!
      use set_nodal_2_each_sf_w_coef
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: ak_e(ele1%numele)
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call vector_phys_2_each_sf_w_coef                                 &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, num_tot_nod_phys, d_nod, ak_e, tensor_sf)
!
      end subroutine tensor_phys_2_each_surf_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_phys_2_each_surf_cst(sf_grp, igrp, k2,          &
     &          i_scalar, coef, scalar_sf)
!
      use set_nodal_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_scalar
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: scalar_sf(sf_grp%num_item)
!
!
      call scalar_phys_2_each_sf_w_const                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_scalar, num_tot_nod_phys, d_nod, coef, scalar_sf)
!
      end subroutine scalar_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      subroutine vector_phys_2_each_surf_cst(sf_grp, igrp, k2,          &
     &          i_vector, coef, vector_sf)
!
      use set_nodal_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_vector
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call vector_phys_2_each_sf_w_const                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_vector, num_tot_nod_phys, d_nod, coef, vector_sf)
!
      end subroutine vector_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      subroutine tensor_phys_2_each_surf_cst(sf_grp, igrp, k2,          &
     &          i_tensor, coef, tensor_sf)
!
      use set_nodal_2_each_sf_w_cst
!
      type(surface_group_data), intent(in) :: sf_grp
      integer (kind = kint), intent(in) :: i_tensor
      integer (kind = kint), intent(in) :: igrp, k2
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: tensor_sf(sf_grp%num_item,6)
!
!
      call tensor_phys_2_each_sf_w_const                                &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    surf1%nnod_4_surf, surf1%node_on_sf,                          &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, i_tensor, num_tot_nod_phys, d_nod, coef, tensor_sf)
!
      end subroutine tensor_phys_2_each_surf_cst
!
! ----------------------------------------------------------------------
!
      end module node_phys_2_each_surface

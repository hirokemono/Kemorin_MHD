!delta_SGS_2_each_surface.f90
!      module delta_SGS_2_each_surface
!
!      Written by H. Matsui on Sep., 2005
!
!!      subroutine d_SGS_flux_2_each_surface                            &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_vect, i_field, i_flux, vector_sf)
!!      subroutine d_SGS_flux_2_each_sf_w_coef                          &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_vect, i_field, i_flux, ak_e,          &
!!     &          vector_sf)
!!      subroutine d_SGS_flux_2_each_sf_w_cst                           &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_vect, i_field, i_flux, coef,          &
!!     &          vector_sf)
!!
!!      subroutine d_SGS_induct_t_2_each_surface                        &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_flux, i_b, i_v, vector_sf)
!!      subroutine d_SGS_induct_t_2_each_sf_w_coef                      &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_flux, i_b, i_v, ak_e, vector_sf)
!!      subroutine d_SGS_induct_t_2_each_sf_w_cst                       &
!!     &         (node, ele, surf, sf_grp, nod_fld,                     &
!!     &          igrp, k2, nd, i_flux, i_b, i_v, coef, vector_sf)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data),    intent(in) :: nod_fld
!
!
      module delta_SGS_2_each_surface
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_flux_2_each_surface                              &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_vect, i_field, i_flux, vector_sf)
!
      use set_delta_SGS_2_each_surf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: i_vect, i_field, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_flux_t_2_each_surface(np_smp, node%numnod,             &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_vect, i_field, i_flux, nod_fld%ntot_phys,     &
     &    nod_fld%d_fld, vector_sf)
!
      end subroutine d_SGS_flux_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_flux_2_each_sf_w_coef                            &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_vect, i_field, i_flux, ak_e,            &
     &          vector_sf)
!
      use set_delta_SGS_2_sf_w_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: igrp, k2, nd
      integer (kind = kint), intent(in) :: i_vect, i_field, i_flux
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_flux_t_2_each_sf_w_coef(np_smp, node%numnod,           &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_vect, i_field, i_flux, nod_fld%ntot_phys,     &
     &    nod_fld%d_fld, ak_e, vector_sf)
!
      end subroutine d_SGS_flux_2_each_sf_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_flux_2_each_sf_w_cst                             &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_vect, i_field, i_flux, coef,            &
     &          vector_sf)
!
      use set_delta_SGS_2_sf_w_cst
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: i_vect, i_field, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_flux_t_2_each_sf_w_cst(np_smp, node%numnod,            &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_vect, i_field, i_flux, nod_fld%ntot_phys,     &
     &    nod_fld%d_fld, coef, vector_sf)
!
      end subroutine d_SGS_flux_2_each_sf_w_cst
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_t_2_each_surface                          &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_flux, i_b, i_v, vector_sf)
!
      use set_delta_SGS_2_each_surf
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_SGS_induct_t_2_surface(np_smp, node%numnod,            &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_flux, i_b, i_v, nod_fld%ntot_phys,            &
     &    nod_fld%d_fld, vector_sf)
!
      end subroutine d_SGS_induct_t_2_each_surface
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_t_2_each_sf_w_coef                        &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_flux, i_b, i_v, ak_e, vector_sf)
!
      use set_delta_SGS_2_sf_w_coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
      real (kind=kreal), intent(in) :: ak_e(ele%numele)
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_SGS_induct_t_2_sf_w_coef(np_smp, node%numnod,          &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_flux, i_b, i_v, nod_fld%ntot_phys,            &
     &    nod_fld%d_fld, ak_e, vector_sf)
!
      end subroutine d_SGS_induct_t_2_each_sf_w_coef
!
! ----------------------------------------------------------------------
!
      subroutine d_SGS_induct_t_2_each_sf_w_cst                         &
     &         (node, ele, surf, sf_grp, nod_fld,                       &
     &          igrp, k2, nd, i_flux, i_b, i_v, coef, vector_sf)
!
      use set_delta_SGS_2_sf_w_cst
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data),    intent(in) :: nod_fld
!
      integer (kind = kint), intent(in) :: i_b, i_v, i_flux
      integer (kind = kint), intent(in) :: igrp, k2, nd
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: vector_sf(sf_grp%num_item,3)
!
!
      call delta_SGS_induct_t_2_sf_w_cst(np_smp, node%numnod,           &
     &    ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    surf%nnod_4_surf, surf%node_on_sf, surf%node_on_sf_n,         &
     &    sf_grp%num_item, sf_grp%item_sf_grp,                          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    igrp, k2, nd, i_flux, i_b, i_v, nod_fld%ntot_phys,            &
     &    nod_fld%d_fld, coef, vector_sf)
!
      end subroutine d_SGS_induct_t_2_each_sf_w_cst
!
! ----------------------------------------------------------------------
!
      end module delta_SGS_2_each_surface

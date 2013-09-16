!>@file   int_free_surf_sph.f90
!!@brief  module int_free_surf_sph
!!
!!@author H. Matsui
!!@date Written  by H. Matsui on Dec., 2003
!@n      modified by H. Matsui on Aug., 2005
!
!>@brief  FEM integration for free slip surface
!!
!!
!!@verbatim
!!      subroutine int_free_surf_sph_in(n_int)
!!      subroutine int_free_surf_sph_out(n_int)
!!@endverbatim
!
      module int_free_surf_sph
!
      use m_precision
      use m_constants
!
      use m_node_phys_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_free_surf_sph_out(n_int)
!
      use m_surf_data_torque
      use int_free_slip_surf_sph
!
      integer (kind = kint), intent(in) :: n_int
!
!
      call int_free_slip_surf_sph_out(n_int, ngrp_sf_fr_out,            &
     &    id_grp_sf_fr_out, iphys%i_velo)
!
      end subroutine int_free_surf_sph_out
!
! ----------------------------------------------------------------------
!
      subroutine int_free_surf_sph_in(n_int)
!
      use m_surf_data_torque
      use int_free_slip_surf_sph
!
      integer (kind = kint), intent(in) :: n_int
!
!
      call int_free_slip_surf_sph_in(n_int, ngrp_sf_fr_in,              &
     &    id_grp_sf_fr_in, iphys%i_velo)
!
      end subroutine int_free_surf_sph_in
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_pseudo_vacuum_surf_sph_out(n_int)
!
      use m_surf_data_vector_p
      use int_free_slip_surf_sph
!
      integer (kind = kint), intent(in) :: n_int
!
!
      call int_free_slip_surf_sph_out(n_int, ngrp_sf_qvc_out,           &
     &    id_grp_sf_qvc_out, iphys%i_vecp)
!
      end subroutine int_pseudo_vacuum_surf_sph_out
!
! ----------------------------------------------------------------------
!
      subroutine int_pseudo_vacuum_surf_sph_in(n_int)
!
      use m_surf_data_vector_p
      use int_free_slip_surf_sph
!
      integer (kind = kint), intent(in) :: n_int
!
!
      call int_free_slip_surf_sph_in(n_int, ngrp_sf_qvc_in,             &
     &    id_grp_sf_qvc_in, iphys%i_vecp)
!
      end subroutine int_pseudo_vacuum_surf_sph_in
!
! ----------------------------------------------------------------------
!
      end module int_free_surf_sph

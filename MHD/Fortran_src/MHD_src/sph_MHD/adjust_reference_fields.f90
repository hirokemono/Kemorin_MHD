!>@file   adjust_reference_fields.f90
!!@brief  module adjust_reference_fields
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2015
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd(reftemp_rj)
!!@endverbatim
!
      module adjust_reference_fields
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_reference_fields
!
      use m_sph_spectr_data
      use m_boundary_params_sph_MHD
      use m_spheric_parameter
!
      use set_reference_sph_mhd
!
!      Set reference temperature and adjust boundary conditions
!
      if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
      call allocate_reft_rj_data
      call set_ref_temp_sph_mhd(nidx_rj, r_ICB, r_CMB, ar_1d_rj,        &
     &    sph_bc_T, reftemp_rj)
      call adjust_sph_temp_bc_by_reftemp                                &
     &   (idx_rj_degree_zero, nidx_rj(2), reftemp_rj, sph_bc_T)
!
      end subroutine init_reference_fields
!
! -----------------------------------------------------------------------
!
      end module adjust_reference_fields

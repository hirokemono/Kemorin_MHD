!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine const_radial_mat_sph_mhd(sph_rj)
!!      subroutine const_radial_mat_sph_snap(sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module const_radial_mat_4_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
!
      use calypso_mpi
!
      implicit none
!
      private :: const_radial_matrices_sph
      private :: const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_mhd(sph_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call const_radial_matrices_sph(sph_rj)
!
      if(sph_rj%inod_rj_center .eq. 0) return
      call const_radial_mat_sph_w_center(sph_rj)
!
      end subroutine const_radial_mat_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_snap(sph_rj)
!
      use m_control_parameter
      use m_radial_matrices_sph
      use m_radial_mat_sph_w_center
      use const_r_mat_4_scalar_sph
      use const_r_mat_w_center_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      if (iflag_t_evo_4_velo .lt. id_Crank_nicolson) return
      call alloc_band_mat_sph(ithree, sph_rj, band_p_poisson)
!
      if(iflag_debug .gt. 0)                                            &
     &          write(*,*) 'const_radial_mat_4_press_sph'
      call const_radial_mat_4_press_sph(sph_rj)
!
      if(sph_rj%inod_rj_center .eq. 0) return
!
      if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
      call const_radial_mat_press00_sph(sph_rj,                         &
     &    band_p_poisson%n_vect, band_p_poisson%n_comp,                 &
     &    band_p_poisson%mat)
!
      end subroutine const_radial_mat_sph_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_matrices_sph(sph_rj)
!
      use m_control_parameter
      use m_radial_matrices_sph
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call allocate_velo_mat_sph(sph_rj)
        call alloc_band_mat_sph(ithree, sph_rj, band_p_poisson)
!
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step(sph_rj)
        call const_radial_mat_4_press_sph(sph_rj)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        call const_radial_mat_4_temp_sph(sph_rj)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call const_radial_mat_4_magne_sph(sph_rj)
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        call const_radial_mat_4_composit_sph(sph_rj)
      end if
!
      end subroutine const_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_w_center(sph_rj)
!
      use m_control_parameter
      use m_radial_matrices_sph
      use m_radial_mat_sph_w_center
      use const_r_mat_w_center_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
        call const_radial_mat_press00_sph(sph_rj,                       &
     &      band_p_poisson%n_vect, band_p_poisson%n_comp,               &
     &      band_p_poisson%mat)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_temp00_sph'
        call allocate_temp00_mat_sph(sph_rj)
        call const_radial_mat_temp00_sph(sph_rj,                        &
     &      band_temp_evo%n_vect, band_temp_evo%n_comp,                 &
     &      band_temp_evo%mat)
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_comp00_sph'
        call allocate_comp00_mat_sph(sph_rj)
        call const_radial_mat_comp00_sph(sph_rj,                        &
     &      band_comp_evo%n_vect, band_comp_evo%n_comp,                 &
     &      band_comp_evo%mat)
      end if
!
      end subroutine const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph

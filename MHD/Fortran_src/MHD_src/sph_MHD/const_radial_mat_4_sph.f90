!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine const_radial_mat_sph_mhd(sph_rj, r_2nd, leg)
!!      subroutine const_radial_mat_sph_snap(sph_rj, r_2nd, leg)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
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
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
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
      subroutine const_radial_mat_sph_mhd(sph_rj, r_2nd, leg)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
!
      call const_radial_matrices_sph(sph_rj, r_2nd, leg%g_sph_rj)
!
      if(sph_rj%inod_rj_center .gt. 0) then
        call const_radial_mat_sph_w_center(sph_rj)
      end if
!
      end subroutine const_radial_mat_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_snap(sph_rj, r_2nd, leg)
!
      use m_control_parameter
      use m_radial_matrices_sph
      use m_radial_mat_sph_w_center
      use const_r_mat_4_scalar_sph
      use const_r_mat_w_center_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
!
      if (iflag_t_evo_4_velo .lt. id_Crank_nicolson) return
      if(iflag_debug .gt. 0)                                            &
     &          write(*,*) 'const_radial_mat_4_press_sph'
      call const_radial_mat_4_press_sph                                 &
     &   (sph_rj, r_2nd, leg%g_sph_rj, band_p_poisson)
!
      if(sph_rj%inod_rj_center .eq. 0) return
!
      if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
      call const_radial_mat_press00_sph(sph_rj,                         &
     &    band_p_poisson%n_vect, band_p_poisson%n_comp,                 &
     &    band_p_poisson%mat, band_p00_poisson)
!
      end subroutine const_radial_mat_sph_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_matrices_sph(sph_rj, r_2nd, g_sph_rj)
!
      use m_control_parameter
      use m_radial_matrices_sph
      use m_boundary_params_sph_MHD
      use m_physical_property
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step(sph_rj, r_2nd, g_sph_rj,       &
     &      band_vs_poisson, band_vp_evo, band_vt_evo, band_wt_evo)
        call const_radial_mat_4_press_sph                               &
     &     (sph_rj, r_2nd, g_sph_rj, band_p_poisson)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        write(band_temp_evo%mat_name,'(a)') 'Temperature_evolution'
        call const_radial_mat_4_scalar_sph                              &
     &     (sph_rj, r_2nd, sph_bc_T, g_sph_rj, coef_imp_t,              &
     &      coef_temp, coef_d_temp, band_temp_evo)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call const_radial_mat_4_magne_sph                               &
     &     (sph_rj, r_2nd, g_sph_rj, band_bp_evo, band_bt_evo)
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        write(band_comp_evo%mat_name,'(a)') 'Composition_evolution'
        call const_radial_mat_4_scalar_sph                              &
     &     (sph_rj, r_2nd, sph_bc_C, g_sph_rj, coef_imp_c,              &
     &      coef_light, coef_d_light, band_comp_evo)
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
      use m_boundary_params_sph_MHD
      use m_physical_property
      use const_r_mat_w_center_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call allocate_average_w_center(sph_rj)
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        if(i_debug .gt. 0) write(*,*) 'const_radial_mat_press00_sph'
        write(band_p_poisson%mat_name,'(a)')                            &
     &                         'average_pressure_w_center'
        call const_radial_mat_press00_sph(sph_rj,                       &
     &      band_p_poisson%n_vect, band_p_poisson%n_comp,               &
     &      band_p_poisson%mat, band_p00_poisson)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_temp00_sph'
        write(band_temp_evo%mat_name,'(a)')                             &
     &                         'average_temperature_w_center'
        call const_radial_mat_scalar00_sph(sph_rj, sph_bc_T,            &
     &      coef_imp_t, coef_temp, coef_d_temp,                         &
     &      band_temp_evo%n_vect, band_temp_evo%n_comp,                 &
     &      band_temp_evo%mat, band_temp00_evo)
      end if
!
      if(iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
          if(i_debug .gt. 0) write(*,*) 'const_radial_mat_comp00_sph'
        write(band_comp_evo%mat_name,'(a)')                             &
     &                        'average_composition_w_center'
        call const_radial_mat_scalar00_sph(sph_rj, sph_bc_C,            &
     &      coef_imp_c, coef_light, coef_d_light,                       &
     &      band_comp_evo%n_vect, band_comp_evo%n_comp,                 &
     &      band_comp_evo%mat, band_comp00_evo)
      end if
!
      end subroutine const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph

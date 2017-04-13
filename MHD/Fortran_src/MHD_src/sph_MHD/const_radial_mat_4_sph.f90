!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine const_radial_mat_sph_mhd                             &
!!     &        (dt, MHD_prop, sph_MHD_bc, sph_rj, r_2nd, leg)
!!      subroutine const_radial_mat_sph_snap                            &
!!     &         (MHD_prop, sph_MHD_bc, sph_rj, r_2nd, leg)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
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
      use t_control_parameter
      use t_physical_property
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_physical_property
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_matrices
      use t_sph_matrix
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
      subroutine const_radial_mat_sph_mhd                               &
     &        (dt, MHD_prop, sph_MHD_bc, sph_rj, r_2nd, leg)
!
      use m_radial_matrices_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
      real(kind = kreal), intent(in) :: dt
!
!
      call const_radial_matrices_sph                                    &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B,                     &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    sph_rj, r_2nd, leg%g_sph_rj, dt)
!
      if(sph_rj%inod_rj_center .gt. 0) then
        call const_radial_mat_sph_w_center                              &
     &     (dt, sph_rj, MHD_prop, sph_MHD_bc,                           &
     &      band_p_poisson, band_temp_evo, band_comp_evo,               &
     &      sph_MHD_mat1)
      end if
!
      end subroutine const_radial_mat_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_snap                              &
     &         (MHD_prop, sph_MHD_bc, sph_rj, r_2nd, leg)
!
      use m_radial_matrices_sph
      use t_boundary_data_sph_MHD
      use const_r_mat_4_scalar_sph
      use const_r_mat_w_center_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
      character(len=kchara) :: mat_name
!
!
      if(MHD_prop%fl_prop%iflag_scheme .lt. id_Crank_nicolson) return
      if(iflag_debug .gt. 0)                                            &
     &          write(*,*) 'const_radial_mat_4_press_sph'
      call const_radial_mat_4_press_sph(sph_rj, r_2nd,                  &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, leg%g_sph_rj,          &
     &    band_p_poisson)
!
      if(sph_rj%inod_rj_center .eq. 0) return
!
      write(mat_name,'(a)') 'average_pressure_w_center'
      call const_radial_mat_press00_sph                                 &
     &   (mat_name, sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,      &
     &    band_p_poisson, sph_MHD_mat1%band_p00_poisson)
!
      end subroutine const_radial_mat_sph_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_matrices_sph                              &
     &         (fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          sph_bc_U, sph_bc_B, sph_bc_T, sph_bc_C,                 &
     &          sph_rj, r_2nd, g_sph_rj, dt)
!
      use m_radial_matrices_sph
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_boundary_type), intent(in) :: sph_bc_T, sph_bc_C
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
!
      if (fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step                                &
     &     (sph_rj, r_2nd, fl_prop, sph_bc_U, g_sph_rj, dt,             &
     &      band_vs_poisson, band_vp_evo, band_vt_evo, band_wt_evo)
        call const_radial_mat_4_press_sph                               &
     &     (sph_rj, r_2nd, fl_prop, sph_bc_U, g_sph_rj, band_p_poisson)
      end if
!
      if (ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_temp_sph'
        write(band_temp_evo%mat_name,'(a)') 'Temperature_evolution'
        call const_radial_mat_4_scalar_sph                              &
     &     (sph_rj, r_2nd, sph_bc_T, g_sph_rj, dt, ht_prop%coef_imp,    &
     &      ht_prop%coef_advect, ht_prop%coef_diffuse, band_temp_evo)
      end if
!
      if (cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_magne_sph'
        call const_radial_mat_4_magne_sph(sph_rj, r_2nd,                &
     &      cd_prop, sph_bc_B, g_sph_rj, dt, band_bp_evo, band_bt_evo)
      end if
!
      if(cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
          if(iflag_debug .gt. 0)                                        &
     &          write(*,*) 'const_radial_mat_4_composit_sph'
        write(band_comp_evo%mat_name,'(a)') 'Composition_evolution'
        call const_radial_mat_4_scalar_sph                              &
     &     (sph_rj, r_2nd, sph_bc_C, g_sph_rj, dt, cp_prop%coef_imp,    &
     &      cp_prop%coef_advect, cp_prop%coef_diffuse, band_comp_evo)
      end if
!
      end subroutine const_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_w_center                          &
     &        (dt, sph_rj, MHD_prop, sph_MHD_bc,                        &
     &         band_p_poisson, band_temp_evo, band_comp_evo,            &
     &         sph_MHD_mat)
!
      use const_r_mat_w_center_sph
!
      real(kind = kreal), intent(in) :: dt
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(band_matrices_type), intent(in) :: band_temp_evo
      type(band_matrices_type), intent(in) :: band_comp_evo
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
      character(len=kchara) :: mat_name
!
!
      call alloc_average_w_center(sph_rj, sph_MHD_mat)
!
      write(mat_name,'(a)') 'average_pressure_w_center'
      call const_radial_mat_press00_sph                                 &
     &   (mat_name, sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,      &
     &    band_p_poisson, sph_MHD_mat%band_p00_poisson)
!
      write(mat_name,'(a)') 'average_temperature_w_center'
      call const_radial_mat_scalar00_sph                                &
     &   (mat_name, dt, sph_rj, MHD_prop%ht_prop, sph_MHD_bc%sph_bc_T,  &
     &    band_temp_evo, sph_MHD_mat%band_temp00_evo)
!
      write(mat_name,'(a)') 'average_composition_w_center'
      call const_radial_mat_scalar00_sph                                &
     &   (mat_name, dt, sph_rj, MHD_prop%cp_prop, sph_MHD_bc%sph_bc_C,  &
     &    band_comp_evo, sph_MHD_mat%band_comp00_evo)
!
      end subroutine const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph

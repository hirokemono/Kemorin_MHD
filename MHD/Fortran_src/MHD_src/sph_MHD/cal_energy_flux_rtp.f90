!> @file  cal_energy_flux_rtp.f90
!!      module cal_energy_flux_rtp
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2009
!! @n    Modified in Apr., 2013
!
!> @brief Evaluate energy fluxes for MHD dynamo in physical space
!!
!!@verbatim
!!      subroutine cal_nonlinear_pole_MHD(sph_rtp, MHD_prop,            &
!!     &          f_trns, bs_trns, trns_b_MHD, trns_f_MHD)
!!      subroutine s_cal_energy_flux_rtp(sph_rtp,                       &
!!     &          fl_prop, cd_prop, ref_param_T, ref_param_C, leg,      &
!!     &          f_trns, bs_trns, be_trns, fs_trns, bs_trns_diff_v,    &
!!     &          trns_f_MHD, trns_b_snap, trns_b_eflux, trns_b_difv,   &
!!     &          trns_f_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: f_trns
!!        type(phys_address), intent(in) :: bs_trns, fs_trns
!!        type(phys_address), intent(in) :: be_trns
!!        type(spherical_transform_data), intent(in) :: trns_f_MHD
!!        type(spherical_transform_data), intent(in) :: trns_b_snap
!!        type(spherical_transform_data), intent(in) :: trns_b_eflux
!!        type(spherical_transform_data), intent(inout) :: trns_f_snap
!!
!!      subroutine cal_buoyancy_flux_rtp_smp(np_smp, nnod, nr,          &
!!     &          inod_smp_stack, radius, coef, scalar, vr, prod)
!!@endverbatim
!
      module cal_energy_flux_rtp
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_phys_address
      use t_spheric_rtp_data
      use t_physical_property
      use t_reference_scalar_param
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: cal_wz_coriolis_pole
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_nonlinear_pole_MHD(sph_rtp, MHD_prop,              &
     &          b_trns, f_trns, trns_b_MHD, trns_f_MHD)
!
      use cal_nonlinear_sph_MHD
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: b_trns
      type(spherical_transform_data), intent(in) :: trns_b_MHD
!
      type(spherical_transform_data), intent(inout) :: trns_f_MHD
!
!
      call nonlinear_terms_on_node(MHD_prop,                            &
     &    b_trns%base, f_trns%forces, sph_rtp%nnod_pole,                &
     &    trns_b_MHD%ncomp, trns_b_MHD%fld_pole,                        &
     &    trns_f_MHD%ncomp, trns_f_MHD%fld_pole)
!
      if(MHD_prop%fl_prop%iflag_4_coriolis) then
        call cal_wz_coriolis_pole                                       &
     &     (sph_rtp%nnod_pole, MHD_prop%fl_prop%coef_cor,               &
     &      trns_b_MHD%fld_pole(1,b_trns%base%i_velo),                  &
     &      trns_f_MHD%fld_pole(1,f_trns%forces%i_coriolis))
      end if
!
      end subroutine cal_nonlinear_pole_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_wz_coriolis_pole                                   &
     &         (nnod_pole,  coef_cor, velo_pole, coriolis_pole)
!
      integer(kind = kint), intent(in) :: nnod_pole
      real(kind = kreal), intent(in) :: coef_cor
      real(kind = kreal), intent(in) :: velo_pole(nnod_pole,3)
!
      real(kind = kreal), intent(inout) :: coriolis_pole(nnod_pole,3)
!
      integer(kind = kint) :: inod
      real(kind = kreal), parameter :: omega(3) = (/zero, zero, one/)
!
!
!$omp parallel do private(inod)
      do inod = 1, nnod_pole
            coriolis_pole(inod,1) = - coef_cor                          &
!     &                         * ( omega(2)*velo_pole(inod,3)          &
     &                         * ( -omega(3)*velo_pole(inod,2) )
            coriolis_pole(inod,2) = - coef_cor                          &
     &                         * ( omega(3)*velo_pole(inod,1) )
!     &                           - omega(1)*velo_pole(inod,3) )
            coriolis_pole(inod,3) = zero
!            coriolis_pole(inod,3) = - coef_cor                         &
!     &                         * ( omega(1)*velo_pole(inod,2)          &
!     &                           - omega(2)*velo_pole(inod,1) )
      end do
!$omp end parallel do
!
      end subroutine cal_wz_coriolis_pole
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rtp(sph_rtp,                         &
     &          fl_prop, cd_prop, ref_param_T, ref_param_C, leg,        &
     &          f_trns, bs_trns, be_trns, fs_trns, bs_trns_diff_v,      &
     &          trns_f_MHD, trns_b_snap, trns_b_eflux, trns_b_difv,     &
     &          trns_f_snap)
!
      use poynting_flux_smp
      use sph_transforms_4_MHD
      use mag_of_field_smp
      use const_wz_coriolis_rtp
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: f_trns
      type(phys_address), intent(in) :: bs_trns, fs_trns
      type(phys_address), intent(in) :: be_trns
      type(diff_vector_address), intent(in) :: bs_trns_diff_v
      type(spherical_transform_data), intent(in) :: trns_f_MHD
      type(spherical_transform_data), intent(in) :: trns_b_snap
      type(spherical_transform_data), intent(in) :: trns_b_eflux
      type(spherical_transform_data), intent(in) :: trns_b_difv
!
      type(spherical_transform_data), intent(inout) :: trns_f_snap
!
!
      if(fs_trns%forces%i_coriolis .gt. 0) then
        call cal_wz_coriolis_rtp(sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,    &
     &      leg%g_colat_rtp, fl_prop%coef_cor,                          &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%forces%i_coriolis))
      end if
!
!$omp parallel
      if(fs_trns%ene_flux%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_f_MHD%fld_rtp(1,f_trns%forces%i_lorentz),              &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_ujb) )
      end if
!
      if(fs_trns%ene_flux%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(sph_rtp%nnod_rtp, dminus,          &
     &      trns_f_MHD%fld_rtp(1,f_trns%forces%i_lorentz),              &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_nega_ujb))
      end if
!
      if(fs_trns%prod_fld%i_k_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_k_heli))
      end if
      if(fs_trns%prod_fld%i_c_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_c_heli))
      end if
      if(fs_trns%prod_fld%i_x_heli .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_x_heli))
      end if
!
      if(fs_trns%ene_flux%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      trns_b_eflux%fld_rtp(1,be_trns%forces%i_induction),         &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_me_gen))
      end if
      if(fs_trns%prod_fld%i_electric .gt. 0) then
        call cal_electric_field_smp(np_smp, sph_rtp%nnod_rtp,           &
     &      sph_rtp%istack_inod_rtp_smp, cd_prop%coef_diffuse,          &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_f_MHD%fld_rtp(1,f_trns%forces%i_vp_induct),            &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_electric))
      end if
!
      if(fs_trns%prod_fld%i_poynting .gt. 0) then
        call cal_poynting_flux_smp(np_smp, sph_rtp%nnod_rtp,            &
     &      sph_rtp%istack_inod_rtp_smp, cd_prop%coef_diffuse,          &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_f_MHD%fld_rtp(1,f_trns%forces%i_vp_induct),            &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_poynting))
      end if
!
      if(fs_trns%ene_flux%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &       sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,          &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_buo,                &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_per_temp),           &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),               &
     &        trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r,  fl_prop%coef_buo,               &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_temp),               &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),               &
     &        trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_buo_gen))
        end if
      end if
!
      if(fs_trns%ene_flux%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_per_light),          &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),               &
     &        trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_c_buo_gen))
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, sph_rtp%nnod_rtp,      &
     &        sph_rtp%nidx_rtp(1), sph_rtp%istack_inod_rtp_smp,         &
     &        sph_rtp%radius_1d_rtp_r, fl_prop%coef_comp_buo,           &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_light),              &
     &        trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),               &
     &        trns_f_snap%fld_rtp(1,fs_trns%ene_flux%i_c_buo_gen) )
        end if
      end if
!
      if(fs_trns%prod_fld%i_velo_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &      (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,     &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_vort),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_velo_scale))
      end if
      if(fs_trns%prod_fld%i_magne_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_magne_scale))
      end if
      if(fs_trns%prod_fld%i_temp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_temp),                 &
     &      trns_b_eflux%fld_rtp(1,be_trns%diffusion%i_t_diffuse),      &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_temp_scale))
      end if
      if(fs_trns%prod_fld%i_comp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_light),                &
     &      trns_b_eflux%fld_rtp(1,be_trns%diffusion%i_c_diffuse),      &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_comp_scale))
      end if
!$omp end parallel
!
!$omp parallel
      if(fs_trns%prod_fld%i_square_v .gt. 0) then
        call vector_vector_prod_smp(sph_rtp%nnod_rtp,                   &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_v))
      end if
      if(fs_trns%prod_fld%i_square_w .gt. 0) then
        call vector_vector_prod_smp(sph_rtp%nnod_rtp,                   &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_vort),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_vort),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_w))
      end if
      if(fs_trns%prod_fld%i_square_b .gt. 0) then
        call vector_vector_prod_smp(sph_rtp%nnod_rtp,                   &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_b))
      end if
      if(fs_trns%prod_fld%i_square_a .gt. 0) then
        call vector_vector_prod_smp(sph_rtp%nnod_rtp,                   &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_vecp),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_vecp),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_a))
      end if
      if(fs_trns%prod_fld%i_square_j .gt. 0) then
        call vector_vector_prod_smp(sph_rtp%nnod_rtp,                   &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_current),              &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_j))
      end if
      if(fs_trns%prod_fld%i_square_t .gt. 0) then
        call cal_scalar_prod_no_coef_smp(sph_rtp%nnod_rtp,              &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_temp),                 &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_temp),                 &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_t))
      end if
      if(fs_trns%prod_fld%i_square_c .gt. 0) then
        call cal_scalar_prod_no_coef_smp(sph_rtp%nnod_rtp,              &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_light),                &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_light),                &
     &      trns_f_snap%fld_rtp(1,fs_trns%prod_fld%i_square_c))
      end if
!$omp end parallel
!
      if(fs_trns%forces%i_mag_stretch .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rtp_magnetic_streach'
!$omp parallel
        call cal_rtp_magnetic_streach                                   &
     &     (np_smp, sph_rtp%nnod_rtp, sph_rtp%istack_inod_rtp_smp,      &
     &      sph_rtp%nidx_rtp(1), sph_rtp%nidx_rtp(2),                   &
     &      sph_rtp%a_r_1d_rtp_r, sph_rtp%cot_theta_1d_rtp,             &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_magne),                &
     &      trns_b_snap%fld_rtp(1,bs_trns%base%i_velo),                 &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vx),            &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vy),            &
     &      trns_b_difv%fld_rtp(1,bs_trns_diff_v%i_grad_vz),            &
     &      trns_f_snap%fld_rtp(1,fs_trns%forces%i_mag_stretch))
!$omp end parallel
      end if
!
      end subroutine s_cal_energy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_smp(np_smp, nnod, nr,            &
     &          inod_smp_stack, radius, coef, scalar, vr, prod)
!
      integer (kind=kint), intent(in) :: np_smp, nnod, nr
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: coef, scalar(nnod), vr(nnod)
      real (kind=kreal), intent(in) :: radius(nr)
!
      real (kind=kreal), intent(inout) :: prod(nnod)
!
      integer (kind=kint) :: iproc, inod, ist, ied, k
!
!
!$omp do private(inod,ist,ied,k)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          k = mod( (inod-1),nr) + 1
          prod(inod) =  coef*scalar(inod)*vr(inod)*radius(k)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_buoyancy_flux_rtp_smp
!
! -----------------------------------------------------------------------
!
      end module cal_energy_flux_rtp

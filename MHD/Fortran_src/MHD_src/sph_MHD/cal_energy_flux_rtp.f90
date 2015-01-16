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
!!      subroutine s_cal_energy_flux_rtp
!!@endverbatim
!
      module cal_energy_flux_rtp
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: cal_buoyancy_flux_rtp_smp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_energy_flux_rtp
!
      use m_machine_parameter
      use m_control_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_physical_property
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_addresses_trans_sph_tmp
      use m_work_4_sph_trans
      use poynting_flux_smp
      use sph_poynting_flux_smp
      use sph_transforms_4_MHD
      use mag_of_field_smp
      use const_wz_coriolis_rtp
      use cal_products_smp
!
!
!$omp parallel
!      if(fsnap_trns%i_coriolis .gt. 0) then
!        call cal_wz_coriolis_rtp(nnod_rtp, fld_rtp(1,b_trns%i_velo),   &
!            frc_snap_rtp(1,fsnap_trns%i_Coriolis))
!      end if
!
      if(fsnap_trns%i_ujb .gt. 0) then
        call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, frc_MHD_rtp(1,f_trns%i_lorentz),        &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_ujb) )
      end if
!
      if(fsnap_trns%i_nega_ujb .gt. 0) then
        call cal_dot_prod_w_coef_smp(np_smp, nnod_rtp,                  &
     &      inod_rtp_smp_stack, dminus,                                 &
     &      frc_MHD_rtp(1,f_trns%i_lorentz),                            &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_nega_ujb) )
      end if
!
      if(fsnap_trns%i_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(np_smp, nnod_rtp,                 &
     &      inod_rtp_smp_stack, fld_snap_rtp(1,bsnap_trns%i_induction), &
     &      fld_snap_rtp(1,bsnap_trns%i_magne),                         &
     &      frc_snap_rtp(1,fsnap_trns%i_me_gen))
      end if
!
      if(fsnap_trns%i_electric .gt. 0) then
        call cal_electric_field_smp                                     &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack, coef_d_magne,         &
     &      fld_snap_rtp(1,bsnap_trns%i_current),                       &
     &      frc_MHD_rtp(1,f_trns%i_vp_induct),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_electric))
      end if
!
      if(fsnap_trns%i_poynting .gt. 0) then
        call cal_poynting_flux_smp                                      &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack, coef_d_magne,         &
     &      fld_snap_rtp(1,bsnap_trns%i_current),                       &
     &      frc_MHD_rtp(1,f_trns%i_vp_induct),                          &
     &      fld_snap_rtp(1,bsnap_trns%i_magne),                         &
     &      frc_snap_rtp(1,fsnap_trns%i_poynting))
      end if
!
      if(fsnap_trns%i_buo_gen .gt. 0) then
        if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        fld_snap_rtp(1,bsnap_trns%i_par_temp),                    &
     &        fld_snap_rtp(1,bsnap_trns%i_velo),                        &
     &        frc_snap_rtp(1,fsnap_trns%i_buo_gen) )
        else
          call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1), &
     &        inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,            &
     &        fld_snap_rtp(1,bsnap_trns%i_temp),                        &
     &        fld_snap_rtp(1,bsnap_trns%i_velo),                        &
     &        frc_snap_rtp(1,fsnap_trns%i_buo_gen) )
        end if
      end if
!
      if(fsnap_trns%i_c_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_comp_buo,         &
     &      fld_snap_rtp(1,bsnap_trns%i_light),                         &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_c_buo_gen) )
      end if
!
      if(fsnap_trns%i_f_buo_gen .gt. 0) then
        call cal_buoyancy_flux_rtp_smp(np_smp, nnod_rtp, nidx_rtp(1),   &
     &      inod_rtp_smp_stack, radius_1d_rtp_r, coef_buo,              &
     &      fld_snap_rtp(1,bsnap_trns%i_filter_temp),                   &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_f_buo_gen) )
      end if
!
      if(fsnap_trns%i_velo_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &      (np_smp, nnod_rtp, inod_rtp_smp_stack,                      &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      fld_snap_rtp(1,bsnap_trns%i_vort),                          &
     &      frc_snap_rtp(1,fsnap_trns%i_velo_scale))
      end if
      if(fsnap_trns%i_magne_scale .gt. 0) then
        call cal_len_scale_by_rot_smp                                   &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      fld_snap_rtp(1,bsnap_trns%i_magne),                         &
     &      fld_snap_rtp(1,bsnap_trns%i_current),                       &
     &      frc_snap_rtp(1,fsnap_trns%i_magne_scale))
      end if
      if(fsnap_trns%i_temp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      fld_snap_rtp(1,bsnap_trns%i_temp),                          &
     &      fld_snap_rtp(1,bsnap_trns%i_t_diffuse),                     &
     &      frc_snap_rtp(1,fsnap_trns%i_temp_scale))
      end if
      if(fsnap_trns%i_comp_scale .gt. 0) then
        call cal_len_scale_by_diffuse_smp                               &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      fld_snap_rtp(1,bsnap_trns%i_light),                         &
     &      fld_snap_rtp(1,bsnap_trns%i_c_diffuse),                     &
     &      frc_snap_rtp(1,fsnap_trns%i_comp_scale))
      end if
!$omp end parallel
!
!
      if(fsnap_trns%i_mag_stretch .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_rtp_magnetic_streach'
!$omp parallel
        call cal_rtp_magnetic_streach                                   &
     &     (np_smp, nnod_rtp, inod_rtp_smp_stack,                       &
     &      nidx_rtp(1), nidx_rtp(2), a_r_1d_rtp_r, cot_theta_1d_rtp,   &
     &      fld_snap_rtp(1,bsnap_trns%i_magne),                         &
     &      fld_snap_rtp(1,bsnap_trns%i_velo),                          &
     &      fld_tmp_rtp(1,btmp_trns%i_grad_vx),                         &
     &      fld_tmp_rtp(1,btmp_trns%i_grad_vy),                         &
     &      fld_tmp_rtp(1,btmp_trns%i_grad_vz),                         &
     &      frc_snap_rtp(1,fsnap_trns%i_mag_stretch) )
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

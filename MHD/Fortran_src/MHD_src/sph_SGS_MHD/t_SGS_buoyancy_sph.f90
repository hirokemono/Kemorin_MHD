!>@file   t_SGS_buoyancy_sph.f90
!!@brief  module t_SGS_buoyancy_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2016
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine alloc_sph_ave_Csim_SGS_buo                           &
!!     &         (sph_rj, sph_rtp, wk_sgs_buo)
!!      subroutine dealloc_sph_ave_Csim_SGS_buo(wk_sgs_buo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!!
!!      subroutine cal_volume_4__SGS_buoyancy                           &
!!     &         (sph_params, sph_rj, wk_sgs_buo)
!!      subroutine sphere_averaged_SGS_buoyancy                         &
!!     &         (sph_rj, sph_rtp, ipol, rj_fld, trns_SGS, wk_sgs_buo)
!!      subroutine volume_averaged_SGS_buoyancy                         &
!!     &         (sph_params, sph_rj, ipol, rj_fld, wk_sgs_buo)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!!
!!      subroutine magnify_sph_ave_SGS_buoyancy(sph_rj, sph_rtp, ipol,  &
!!     &          ifld_sgs, wk_sgs_buo, rj_fld, trns_SGS)
!!      subroutine magnify_vol_ave_SGS_buoyancy                         &
!!     &         (sph_rtp, ipol, ifld_sgs, wk_sgs_buo, rj_fld, trns_SGS)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_terms_address), intent(in) :: ifld_sgs
!!        type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!@endverbatim
!
      module t_SGS_buoyancy_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_spheric_rtp_data
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
!
      implicit none
!
!>      Work area for SGS buoyancy
      type work_4_sph_SGS_buoyancy
!         horizontal average for SGS bouyancies
        real(kind = kreal), allocatable :: Cbuo_ave_sph_lc(:,:)
!         horizontal average for SGS bouyancies
        real(kind = kreal), allocatable :: Cbuo_ave_sph_gl(:,:)
!
!         horizontal average for SGS bouyancies
        real(kind = kreal), allocatable :: Cbuo_ave_sph_rtp(:,:)
!
!         local voulme average for SGS bouyancies
        real(kind = kreal) :: Cbuo_vol_lc(2)
!         global voulme average for SGS bouyancies
        real(kind = kreal) :: Cbuo_vol_gl(2)
!
!          1 / volume for SGS bouyancies
        real(kind = kreal) :: avol_SGS_buo
      end type work_4_sph_SGS_buoyancy
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sph_ave_Csim_SGS_buo                             &
     &         (sph_rj, sph_rtp, wk_sgs_buo)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
      allocate(wk_sgs_buo%Cbuo_ave_sph_lc(0:sph_rj%nidx_rj(1),2))
      allocate(wk_sgs_buo%Cbuo_ave_sph_gl(0:sph_rj%nidx_rj(1),2))
      allocate(wk_sgs_buo%Cbuo_ave_sph_rtp(sph_rtp%nidx_rtp(1),2))
      wk_sgs_buo%Cbuo_ave_sph_lc = 0.0d0
      wk_sgs_buo%Cbuo_ave_sph_gl = 0.0d0
      wk_sgs_buo%Cbuo_ave_sph_rtp = 0.0d0
!
      end subroutine alloc_sph_ave_Csim_SGS_buo
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_sph_ave_Csim_SGS_buo(wk_sgs_buo)
!
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
      deallocate(wk_sgs_buo%Cbuo_ave_sph_lc)
      deallocate(wk_sgs_buo%Cbuo_ave_sph_gl)
      deallocate(wk_sgs_buo%Cbuo_ave_sph_rtp)
!
      end subroutine dealloc_sph_ave_Csim_SGS_buo
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_volume_4__SGS_buoyancy                             &
     &         (sph_params, sph_rj, wk_sgs_buo)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
!
      wk_sgs_buo%avol_SGS_buo = (one / three)                           &
     &      * (sph_rj%radius_1d_rj_r(sph_params%nlayer_CMB)**3          &
     &       - sph_rj%radius_1d_rj_r(sph_params%nlayer_ICB)**3)
      wk_sgs_buo%avol_SGS_buo = one / wk_sgs_buo%avol_SGS_buo
!
      end subroutine cal_volume_4__SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine sphere_averaged_SGS_buoyancy                           &
     &         (sph_rj, sph_rtp, ipol, rj_fld, wk_sgs_buo)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
      integer(kind = kint) :: k, k_gl, num
!
!
      wk_sgs_buo%Cbuo_ave_sph_lc(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      wk_sgs_buo%Cbuo_ave_sph_gl(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
!
        if(ipol%i_Csim_SGS_buoyancy .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol%i_Csim_SGS_buoyancy, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(1),         &
     &        itwo, ione, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        if(ipol%i_Csim_SGS_comp_buo .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol%i_Csim_SGS_comp_buo, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(1),         &
     &        itwo, itwo, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
      end if
!
      num = itwo * (sph_rj%nidx_rj(1) + 1)
!
!
      call MPI_allREDUCE                                                &
     &   (wk_sgs_buo%Cbuo_ave_sph_lc, wk_sgs_buo%Cbuo_ave_sph_gl,       &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      do k = 1, sph_rtp%nidx_rtp(1)
        k_gl = sph_rtp%idx_gl_1d_rtp_r(k)
        wk_sgs_buo%Cbuo_ave_sph_rtp(k,1)                                &
     &       = wk_sgs_buo%Cbuo_ave_sph_gl(k_gl,1)
        wk_sgs_buo%Cbuo_ave_sph_rtp(k,2)                                &
     &       = wk_sgs_buo%Cbuo_ave_sph_gl(k_gl,2)
      end do
!
      end subroutine sphere_averaged_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine volume_averaged_SGS_buoyancy                           &
     &         (sph_params, sph_rj, ipol, rj_fld, wk_sgs_buo)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
!
      wk_sgs_buo%Cbuo_vol_lc(1:2) = 0.0d0
      wk_sgs_buo%Cbuo_vol_gl(1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
        wk_sgs_buo%Cbuo_ave_sph_lc(1:sph_rj%nidx_rj(1),1:2) = 0.0d0
!
        if(ipol%i_Csim_SGS_buoyancy .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol%i_Csim_SGS_buoyancy, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1),        &
     &        itwo, ione, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        if(ipol%i_Csim_SGS_comp_buo .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol%i_Csim_SGS_comp_buo, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1),        &
     &        itwo, itwo, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        call radial_integration                                         &
     &     (sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, itwo,             &
     &      wk_sgs_buo%Cbuo_ave_sph_lc, wk_sgs_buo%Cbuo_vol_lc)
!
         wk_sgs_buo%Cbuo_vol_lc(1:2) = wk_sgs_buo%avol_SGS_buo          &
     &                                * wk_sgs_buo%Cbuo_vol_lc(1:2)
      end if
!
      call MPI_allREDUCE                                                &
     &   (wk_sgs_buo%Cbuo_vol_lc, wk_sgs_buo%Cbuo_vol_gl, itwo,         &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine volume_averaged_SGS_buoyancy
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine magnify_sph_ave_SGS_buoyancy(sph_rj, sph_rtp, ipol,    &
     &          ifld_sgs, wk_sgs_buo, rj_fld, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use t_SGS_model_coefs
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
      use SGS_buo_coefs_sph_MHD
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
!
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
!$omp parallel
      if((ipol%i_Csim_SGS_buoyancy*ipol%i_Csim_SGS_comp_buo) .gt. 0)    &
     &    then
        call prod_dbl_radial_buo_coefs_rj(sph_rj%nidx_rj,               &
     &      wk_sgs_buo%Cbuo_ave_sph_gl, ipol%i_SGS_inertia,             &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      call sel_mag_sph_ave_SGS_buo_rtp                                  &
     &   (sph_rtp, ifld_sgs, wk_sgs_buo%Cbuo_ave_sph_rtp, trns_SGS)
!
      end subroutine magnify_sph_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine magnify_vol_ave_SGS_buoyancy                           &
     &         (sph_rtp, ipol, ifld_sgs, wk_sgs_buo, rj_fld, trns_SGS)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use t_SGS_model_coefs
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: ipol
      type(SGS_terms_address), intent(in) :: ifld_sgs
      type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
!
      type(phys_data), intent(inout) :: rj_fld
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if     (ifld_sgs%i_buoyancy*ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_double_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl, ipol%i_SGS_inertia,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call product_double_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl, trns_SGS%f_trns%i_SGS_inertia,      &
     &      sph_rtp%nnod_rtp, trns_SGS%ncomp_rtp_2_rj,                  &
     &      trns_SGS%frc_rtp)
      else if(ifld_sgs%i_buoyancy .gt. 0) then
        call product_single_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl(1), ipol%i_SGS_inertia,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call product_single_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl(1), trns_SGS%f_trns%i_SGS_inertia,   &
     &      sph_rtp%nnod_rtp, trns_SGS%ncomp_rtp_2_rj,                  &
     &      trns_SGS%frc_rtp)
      else if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call product_single_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl(2), ipol%i_SGS_inertia,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call product_single_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl(2), trns_SGS%f_trns%i_SGS_inertia,   &
     &      sph_rtp%nnod_rtp, trns_SGS%ncomp_rtp_2_rj,                  &
     &      trns_SGS%frc_rtp)
      end if
!
      end subroutine magnify_vol_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      end module t_SGS_buoyancy_sph
 
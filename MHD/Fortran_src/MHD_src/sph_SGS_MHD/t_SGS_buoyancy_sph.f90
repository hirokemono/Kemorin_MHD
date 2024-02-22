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
!!      subroutine cal_volume_4_SGS_buoyancy                            &
!!     &         (sph_params, sph_rj, wk_sgs_buo)
!!      subroutine sphere_averaged_SGS_buoyancy                         &
!!     &         (sph_rj, sph_rtp, ipol_Csim, rj_fld, wk_sgs_buo)
!!      subroutine volume_averaged_SGS_buoyancy                         &
!!     &         (sph_params, sph_rj, ipol_Csim, rj_fld, wk_sgs_buo)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(SGS_term_address), intent(in) :: ipol_Csim
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!!
!!      subroutine magnify_sph_ave_SGS_buoyancy(sph_rtp, iak_sgs_term,  &
!!     &          wk_sgs_buo, fg_trns_LES, trns_f_SGS)
!!      subroutine magnify_vol_ave_SGS_buoyancy(sph_rtp, iak_sgs_term,  &
!!     &          wk_sgs_buo, fg_trns_LES, trns_f_SGS)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
!!        type(phys_address), intent(in) :: fg_trns
!!        type(SGS_model_addresses), intent(in) :: fg_trns_LES
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
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
      use t_phys_data
      use t_SGS_model_coef_labels
      use t_sph_trans_arrays_MHD
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
      subroutine cal_volume_4_SGS_buoyancy                              &
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
      end subroutine cal_volume_4_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      subroutine sphere_averaged_SGS_buoyancy                           &
     &         (sph_rj, sph_rtp, ipol_Csim, rj_fld, wk_sgs_buo)
!
      use calypso_mpi_real
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_term_address), intent(in) :: ipol_Csim
!
      type(phys_data), intent(inout) :: rj_fld
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
      integer(kind = kint) :: k, k_gl
      integer(kind = kint_gl) :: num64
!
!
      wk_sgs_buo%Cbuo_ave_sph_lc(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      wk_sgs_buo%Cbuo_ave_sph_gl(0:sph_rj%nidx_rj(1),1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
!
        if(ipol_Csim%i_SGS_buoyancy .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol_Csim%i_SGS_buoyancy, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(1),         &
     &        itwo, ione, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        if(ipol_Csim%i_SGS_comp_buo .gt. 0) then
          call ave_one_scalar_sph_spectr                                &
     &       (ipol_Csim%i_SGS_comp_buo, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r, sph_rj%nidx_rj(1),         &
     &        itwo, itwo, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
      end if
!
      num64 = itwo * (sph_rj%nidx_rj(1) + 1)
      call calypso_mpi_allreduce_real                                   &
     &   (wk_sgs_buo%Cbuo_ave_sph_lc, wk_sgs_buo%Cbuo_ave_sph_gl,       &
     &    num64, MPI_SUM)
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
     &         (sph_params, sph_rj, ipol_Csim, rj_fld, wk_sgs_buo)
!
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_phys_data
      use calypso_mpi_real
      use transfer_to_long_integers
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_SGS_model_coefs_sph
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(SGS_term_address), intent(in) :: ipol_Csim
!
      type(phys_data), intent(inout) :: rj_fld
      type(work_4_sph_SGS_buoyancy), intent(inout) :: wk_sgs_buo
!
      integer(kind = kint) :: kr_inside(2), kr_outside(2)
      real(kind = kreal), parameter :: c_interpolate = one
!
!
      wk_sgs_buo%Cbuo_vol_lc(1:2) = 0.0d0
      wk_sgs_buo%Cbuo_vol_gl(1:2) = 0.0d0
      if(sph_rj%idx_rj_degree_zero .gt. izero) then
        wk_sgs_buo%Cbuo_ave_sph_lc(1:sph_rj%nidx_rj(1),1:2) = 0.0d0
!
        if(ipol_Csim%i_SGS_buoyancy .gt. 0) then
!          write(*,*) 'ave_one_scalar_sph_spectr thermnal'
          call ave_one_scalar_sph_spectr                                &
     &       (ipol_Csim%i_SGS_buoyancy, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1),        &
     &        itwo, ione, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        if(ipol_Csim%i_SGS_comp_buo .gt. 0) then
!          write(*,*) 'ave_one_scalar_sph_spectr composition'
          call ave_one_scalar_sph_spectr                                &
     &       (ipol_Csim%i_SGS_comp_buo, sph_rj%nidx_rj,                 &
     &        sph_rj%idx_rj_degree_zero, sph_rj%inod_rj_center,         &
     &        rj_fld, sph_rj%radius_1d_rj_r,  sph_rj%nidx_rj(1),        &
     &        itwo, itwo, wk_sgs_buo%Cbuo_ave_sph_lc)
        end if
!
        if (iflag_debug.gt.0) write(*,*) 'radial_integration'
!        write(*,*) 'wk_sgs_buo%Cbuo_ave_sph_lc',  &
!     &            wk_sgs_buo%Cbuo_ave_sph_lc
        kr_inside(1:2) =  sph_params%nlayer_ICB
        kr_outside(1:2) = sph_params%nlayer_CMB
        call radial_integration                                         &
     &     (kr_inside, kr_outside, c_interpolate, c_interpolate,        &
     &      sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, itwo,             &
     &      wk_sgs_buo%Cbuo_ave_sph_lc, wk_sgs_buo%Cbuo_vol_lc)
!
         wk_sgs_buo%Cbuo_vol_lc(1:2) = wk_sgs_buo%avol_SGS_buo          &
     &                                * wk_sgs_buo%Cbuo_vol_lc(1:2)
      end if
!
      call calypso_mpi_allreduce_real                                   &
     &   (wk_sgs_buo%Cbuo_vol_lc, wk_sgs_buo%Cbuo_vol_gl,               &
     &    cast_long(2), MPI_SUM)
!      write(*,*) 'wk_sgs_buo%Cbuo_vol_gl', wk_sgs_buo%Cbuo_vol_gl
!
      end subroutine volume_averaged_SGS_buoyancy
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine magnify_vol_ave_SGS_buoyancy(sph_rtp, iak_sgs_term,    &
     &          wk_sgs_buo, fg_trns_LES, trns_f_SGS)
!
      use t_SGS_model_addresses
      use t_rms_4_sph_spectr
      use t_spheric_parameter
      use t_SGS_term_labels
      use radial_int_for_sph_spec
      use volume_average_4_sph
      use prod_buo_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(work_4_sph_SGS_buoyancy), intent(in) :: wk_sgs_buo
      type(SGS_model_addresses), intent(in) :: fg_trns_LES
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if     (iak_sgs_term%i_SGS_buoyancy                               &
     &         * iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call product_double_vol_buo_coefs                               &
     &     (wk_sgs_buo%Cbuo_vol_gl, fg_trns_LES%SGS_term%i_SGS_inertia, &
     &      sph_rtp%nnod_rtp, trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      else if(iak_sgs_term%i_SGS_buoyancy .gt. 0) then
        if(my_rank .eq. 0) write(*,*)                                   &
     &           'product_single_vol_buo_coefs thermal',                &
     &            wk_sgs_buo%Cbuo_vol_gl(1)
        call product_single_vol_buo_coefs(wk_sgs_buo%Cbuo_vol_gl(1),    &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      else if(iak_sgs_term%i_SGS_comp_buo .gt. 0) then
        call product_single_vol_buo_coefs(wk_sgs_buo%Cbuo_vol_gl(2),    &
     &      fg_trns_LES%SGS_term%i_SGS_inertia, sph_rtp%nnod_rtp,       &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      end if
!
      end subroutine magnify_vol_ave_SGS_buoyancy
!
! ----------------------------------------------------------------------
!
      end module t_SGS_buoyancy_sph
 

!!@brief  module zonal_lsq_4_model_coefs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Least square for model coefficients
!!
!!@verbatim
!!      subroutine cal_dynamic_SGS_4_sph_MHD                            &
!!     &        (sph_rtp, sph_d_grp, stab_weight, numdir,               &
!!     &         flux_simi, flux_wide, flux_dble, sgs_zl, sgs_zt, sgs_c)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!@endverbatim
!
      module zonal_lsq_4_model_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_groups_sph_dynamic
      use t_ele_info_4_dynamic
      use t_sph_filtering
!
      implicit none
!
      private :: sel_int_zonal_4_model_coefs
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_dynamic_SGS_4_sph_MHD                              &
     &        (sph_rtp, sph_d_grp, stab_weight, numdir,                 &
     &         flux_simi, flux_wide, flux_dble, sgs_zl, sgs_zt, sgs_c)
!
      use m_FFT_selector
      use cal_sph_model_coefs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      real(kind = kreal), intent(in) :: stab_weight
      integer(kind = kint), intent(in) :: numdir
!
      real(kind = kreal), intent(in)                                    &
     &                   :: flux_simi(sph_rtp%nnod_rtp,numdir)
      real(kind = kreal), intent(in)                                    &
     &                   :: flux_wide(sph_rtp%nnod_rtp,numdir)
      real(kind = kreal), intent(in)                                    &
     &                   :: flux_dble(sph_rtp%nnod_rtp,numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                    :: sgs_zt(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                    :: sgs_c(sph_d_grp%ngrp_dynamic)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_int_zonal_4_model_coefs'
      call sel_int_zonal_4_model_coefs(sph_rtp, sph_d_grp,              &
     &    numdir, flux_simi, flux_wide, flux_dble, sgs_zl, sgs_zt)
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_scalar_sph_model_coefs'
      call cal_scalar_sph_model_coefs                                   &
     &   (sph_d_grp%ngrp_dynamic, stab_weight, sgs_zl, sgs_zt, sgs_c)
!
      end subroutine cal_dynamic_SGS_4_sph_MHD
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_int_zonal_4_model_coefs(sph_rtp, sph_d_grp,        &
     &          numdir, frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
!
      use m_FFT_selector
      use zonal_int_4_sph_Csim_pin
      use zonal_int_4_sph_Csim_pout
!
      integer(kind = kint), intent(in) :: numdir
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!
      real(kind = kreal), intent(in)                                    &
     &                   :: frc_simi(sph_rtp%nnod_rtp,numdir)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_wide(sph_rtp%nnod_rtp,numdir)
      real(kind = kreal), intent(in)                                    &
     &                    :: frc_dble(sph_rtp%nnod_rtp,numdir)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zl(sph_d_grp%ngrp_dynamic)
      real(kind = kreal), intent(inout)                                 &
     &                   :: sgs_zt(sph_d_grp%ngrp_dynamic)
!
!
      if(numdir .eq. n_sym_tensor) then
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call zonal_int_tentor_Csim_pin(sph_rtp, sph_d_grp,            &
     &        frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
        else
          call zonal_int_tentor_Csim_rin(sph_rtp, sph_d_grp,            &
     &        frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
        end if
      else
        if(sph_rtp%istep_rtp(3) .eq. 1) then
          call zonal_int_vector_Csim_pin(sph_rtp, sph_d_grp,            &
     &        frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
        else
          call zonal_int_vector_Csim_rin(sph_rtp, sph_d_grp,            &
     &        frc_simi, frc_wide, frc_dble, sgs_zl, sgs_zt)
        end if
      end if
!
      end subroutine sel_int_zonal_4_model_coefs
!
! ----------------------------------------------------------------------
!
      end module zonal_lsq_4_model_coefs
 

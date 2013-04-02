!cal_div_buoyancies_sph_MHD.f90
!      module cal_div_buoyancies_sph_MHD
!
!        programmed by H.Matsui on July, 2011
!
!      subroutine s_cal_div_buoyancies_sph_MHD
!
      module cal_div_buoyancies_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit  none
!
      private :: cal_div_double_buoyancy_sph_MHD
      private :: cal_div_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_div_buoyancies_sph_MHD
!
      use m_machine_parameter
!
!
      if ( (iflag_4_gravity*iflag_4_composit_buo) .gt. 0) then
!
        if(iflag_4_ref_temp .ne. 100) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_div_double_buoyancy_sph_MHD', ipol%i_temp
          call cal_div_double_buoyancy_sph_MHD(ipol%i_temp,             &
     &        ipol%i_grad_t)
        else
          if (iflag_debug.eq.1) write(*,*)                              &
     &      'cal_div_double_buoyancy_sph_MHD', ipol%i_par_temp
          call cal_div_double_buoyancy_sph_MHD(ipol%i_par_temp,         &
     &        ipol%i_grad_part_t)
        end if
!
      else if (iflag_4_gravity .gt. 0) then
!
        if(iflag_4_ref_temp .ne. 100) then
          if (iflag_debug.eq.1) write(*,*) 'cal_div_buoyancy_sph_MHD'
          call cal_div_buoyancy_sph_MHD(coef_buo, ipol%i_temp,          &
     &        ipol%i_grad_t, ipol%i_div_buoyancy)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_div_buoyancy_sph_MHD'
          call cal_div_buoyancy_sph_MHD(coef_buo,                       &
     &        ipol%i_par_temp, ipol%i_grad_part_t, ipol%i_div_buoyancy)
        end if
!
      else if (iflag_4_composit_buo .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_buoyancy_sph_MHD'
        call cal_div_buoyancy_sph_MHD(coef_comp_buo, ipol%i_light,      &
     &      ipol%i_grad_composit, ipol%i_div_comp_buo)
!
      else if(iflag_4_filter_gravity .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_buoyancy_sph_MHD'
        call cal_div_buoyancy_sph_MHD(coef_buo, ipol%i_filter_temp,     &
     &      ipol%i_grad_filter_temp, ipol%i_div_filter_buo)
      end if
!
      end subroutine s_cal_div_buoyancies_sph_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_buoyancy_sph_MHD(is_t, ids_t)
!
      integer(kind= kint), intent(in) :: is_t, ids_t
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
        ist = (nlayer_ICB-1)*nidx_rj(2) + 1
        ied = nlayer_CMB * nidx_rj(2)
!$omp parallel do private (inod,j,k)
        do inod = ist, ied
          j = mod((inod-1),nidx_rj(2)) + 1
          k = 1 + (inod- j) / nidx_rj(2)
!
          d_rj(inod,ipol%i_div_buoyancy)                                &
     &          = three * (coef_buo * d_rj(inod,is_t)                   &
     &                   + coef_comp_buo * d_rj(inod,ipol%i_light))     &
     &           +  ( coef_buo * d_rj(inod,ids_t)                       &
     &             + coef_comp_buo * d_rj(inod,ipol%i_grad_composit) )  &
     &              * radius_1d_rj_r(k)
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_buoyancy_sph_MHD(coef, is_fld, ids_fld, is_div)
!
      integer(kind= kint), intent(in) :: is_fld, ids_fld, is_div
      real(kind = kreal), intent(in) :: coef
      integer(kind= kint) :: ist, ied, inod, j, k
!
!
        ist = (nlayer_ICB-1)*nidx_rj(2) + 1
        ied = nlayer_CMB * nidx_rj(2)
!$omp parallel do private (inod,j,k)
        do inod = ist, ied
          j = mod((inod-1),nidx_rj(2)) + 1
          k = 1 + (inod- j) / nidx_rj(2)
          d_rj(inod,is_div) = coef * ( three * d_rj(inod,is_fld)        &
     &                       + d_rj(inod,ids_fld) * radius_1d_rj_r(k) )
        end do
!$omp end parallel do
!
      end subroutine cal_div_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      end module cal_div_buoyancies_sph_MHD

!cal_div_r_const_buo_sph.f90
!      module cal_div_r_const_buo_sph
!
!        programmed by H.Matsui on July, 2011
!
!      subroutine cal_div_radial_const_gravity
!
      module cal_div_r_const_buo_sph
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
      private :: cal_div_double_cst_buo_sph
      private :: cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_radial_const_gravity
!
      use m_machine_parameter
!
!
      if ( (iflag_4_gravity*iflag_4_composit_buo) .gt. 0) then
!
        if(iflag_4_ref_temp .ne. 100) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_div_double_cst_buo_sph', ipol%i_temp
          call cal_div_double_cst_buo_sph(ipol%i_temp, ipol%i_grad_t)
        else
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_div_double_cst_buo_sph', ipol%i_par_temp
          call cal_div_double_cst_buo_sph(ipol%i_par_temp,              &
     &        ipol%i_grad_part_t)
        end if
!
      else if (iflag_4_gravity .gt. 0) then
!
        if(iflag_4_ref_temp .ne. 100) then
          if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
          call cal_div_cst_buo_sph(coef_buo, ipol%i_temp,               &
     &        ipol%i_grad_t, ipol%i_div_buoyancy)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
          call cal_div_cst_buo_sph(coef_buo,                            &
     &        ipol%i_par_temp, ipol%i_grad_part_t, ipol%i_div_buoyancy)
        end if
!
      else if (iflag_4_composit_buo .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph(coef_comp_buo, ipol%i_light,           &
     &      ipol%i_grad_composit, ipol%i_div_comp_buo)
!
      else if(iflag_4_filter_gravity .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_div_cst_buo_sph'
        call cal_div_cst_buo_sph(coef_buo, ipol%i_filter_temp,          &
     &      ipol%i_grad_filter_temp, ipol%i_div_filter_buo)
      end if
!
      end subroutine cal_div_radial_const_gravity
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_div_double_cst_buo_sph(is_t, ids_t)
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
     &          = two * a_r_1d_rj_r(k) * (coef_buo * d_rj(inod,is_t)    &
     &                   + coef_comp_buo * d_rj(inod,ipol%i_light))     &
     &           +  ( coef_buo * d_rj(inod,ids_t)                       &
     &             + coef_comp_buo * d_rj(inod,ipol%i_grad_composit) )
        end do
!$omp end parallel do
!
      end subroutine cal_div_double_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_div_cst_buo_sph(coef, is_fld, ids_fld, is_div)
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
          d_rj(inod,is_div) = coef * ( two * d_rj(inod,is_fld)        &
     &                       * a_r_1d_rj_r(k) + d_rj(inod,ids_fld) )
        end do
!$omp end parallel do
!
      end subroutine cal_div_cst_buo_sph
!
!-----------------------------------------------------------------------
!
      end module cal_div_r_const_buo_sph

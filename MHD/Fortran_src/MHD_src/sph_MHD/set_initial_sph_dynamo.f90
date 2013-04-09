!>@file   set_initial_sph_dynamo.f90
!!@brief  module set_initial_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_data_control
!!      subroutine set_noize_scalar_sph(is_fld)
!!      subroutine reduce_initial_magne_sph
!!@endverbatim
!
!
      module set_initial_sph_dynamo
!
      use m_precision
!
      use m_constants
      use m_sph_phys_address
!
      implicit none
!
      private :: set_initial_temp_sph, set_initial_magne_sph
      private :: set_initial_scalar_sph, set_initial_velo_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_control
!
      use m_machine_parameter
      use m_initial_field_control
      use m_t_int_parameter
      use m_t_step_parameter
      use m_field_data_IO
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
      use initial_magne_dynamobench
      use initial_magne_dbench_qvc
!
      integer(kind = kint) :: isig
!
!
      iflag_initial_step = 0
      if (iflag_restart .eq. i_rst_by_file) then
        call read_alloc_sph_restart_data
!
!   for dynamo benchmark
!
      else if(iflag_restart .eq. i_rst_dbench0                          &
     &   .or. iflag_restart .eq. i_rst_dbench1                          &
     &   .or. iflag_restart .eq. i_rst_dbench2                          &
     &   .or. iflag_restart .eq. i_rst_dbench_qcv) then
        isig = 400
        call set_initial_velo_sph
        if(ipol%i_temp .gt. 0) then
          call set_initial_temp_sph(isig)
        end if
        if(ipol%i_light .gt. 0) then
          call set_initial_scalar_sph(isig, ipol%i_light)
        end if
!
        if(iflag_restart .eq. i_rst_dbench1) then
          if(ipol%i_magne .gt. 0) call initial_b_dynamobench_1
        else if(iflag_restart .eq. i_rst_dbench2) then
          if(ipol%i_magne .gt. 0) call initial_b_dynamobench_2
        else if(iflag_restart .eq. i_rst_dbench_qcv) then
          if(ipol%i_magne .gt. 0) call initial_b_dynamobench_qcv
        end if
!
!   set small seed magnetic field
!
      else if (iflag_restart .eq. i_rst_no_file) then
        if(ipol%i_temp .gt. 0)  call set_noize_scalar_sph(ipol%i_temp)
        if(ipol%i_light .gt. 0) call set_noize_scalar_sph(ipol%i_light)
        if(ipol%i_magne .gt. 0) then
          call set_initial_magne_sph(0)
          call reduce_initial_magne_sph
        end if
      end if
!
      call set_sph_restart_num_to_IO
!
      i_step_MHD = i_step_init
      time   =     time_init
!
      if (iflag_restart.ne.i_rst_by_file .and. i_step_init.eq.0) then
        call output_sph_restart_control
      end if
!
      end subroutine sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_sph
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer ( kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo  ) = zero
        d_rj(inod,ipol%i_velo+1) = zero
        d_rj(inod,ipol%i_velo+2) = zero
      end do
!$omp end parallel do
!
      end subroutine set_initial_velo_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp_sph(isig)
!
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer ( kind = kint), intent(in) :: isig
!
      integer ( kind = kint) :: inod, m, j, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
      if (idx_rj_degree_zero .gt. 0) then
        if ( iflag_4_ref_temp .eq. 100 ) then
          do k = 1, nidx_rj(1)
            inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,ipol%i_temp) = reftemp_rj(k,0)
          end do
        else
          do k = 1, nidx_rj(1)
            inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,ipol%i_temp) = (ar_1d_rj(k,1) * 20.d0/13.0d0      &
     &                              - 1.0d0 ) * 7.0d0 / 13.0d0
          end do
        end if
      end if
!
!
!
      m = int( mod(isig,1000) / 100 )
      jj = 0
      do j = 1, nidx_rj(2)
        if (   idx_gl_1d_rj_j(j,2).eq.m                                 &
     &   .and. idx_gl_1d_rj_j(j,3).eq.m) then
         jj = j
         exit
        end if
      end do
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
!
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * 0.1d0 * six / (sqrt(pi))
        end do
      end if
!
      end subroutine set_initial_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_scalar_sph(isig, is_fld)
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer ( kind = kint), intent(in) :: isig, is_fld
!
      integer ( kind = kint) :: inod, m, j, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,is_fld) = zero
      end do
!$omp end parallel do
!
!
      if (idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,is_fld) = reftemp_rj(k,0)
        end do
      end if
!
!
      m = int( mod(isig,1000) / 100 )
      jj = 0
      do j = 1, nidx_rj(2)
        if (   idx_gl_1d_rj_j(j,2).eq.m                                 &
     &   .and. idx_gl_1d_rj_j(j,3).eq.m) then
         jj = j
         exit
        end if
      end do
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
          d_rj(inod,is_fld) = (one-three*xr**2+three*xr**4-xr**6)      &
     &                       * 0.1d0 * six / (sqrt(pi))
        end do
      end if
!
      end subroutine set_initial_scalar_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph(isig)
!
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer ( kind = kint), intent(in) :: isig
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, m, k, j, js, jt
      integer(kind = kint), parameter :: ls = 1, lt = 2
!
!
      m = int(dble( mod(isig,1000) / 100 ))
      js = 0
      jt = 0
      do j = 1, nidx_rj(2)
        if (   idx_gl_1d_rj_j(j,2).eq.ls                                &
     &   .and. idx_gl_1d_rj_j(j,3).eq.0) then
         js = j
         exit
        end if
      end do
      do j = 1, nidx_rj(2)
        if (   idx_gl_1d_rj_j(j,2).eq.lt                                &
     &   .and. idx_gl_1d_rj_j(j,3).eq.0) then
         jt = j
         exit
        end if
      end do
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,ipol%i_magne  ) = zero
        d_rj(is,ipol%i_magne+1) = zero
        d_rj(is,ipol%i_magne+2) = zero
        d_rj(is,ipol%i_current  ) = zero
        d_rj(is,ipol%i_current+1) = zero
        d_rj(is,ipol%i_current+2) = zero
      end do
!$omp end parallel do
!
      if (iflag_icb_magne .eq. iflag_sph_fill_center) then
!
        if (js .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            is = js + (k-1)*nidx_rj(2)
            rr = radius_1d_rj_r(k)
!
            d_rj(is,ipol%i_magne) =  (five / eight) * (-three * rr**3   &
     &                       + four * r_CMB * rr**2 - r_ICB**4 / rr)
            d_rj(is,idpdr%i_magne) = (five / eight) * (-dnine * rr**2   &
     &                         + eight * r_CMB * rr + r_ICB**4 / rr**2)
            d_rj(is,itor%i_current) =  (five*three / two) * rr
          end do
        end if
!
        if (jt .gt. 0) then
          do k = nlayer_ICB, nlayer_CMB
            it = jt + (k-1)*nidx_rj(2)
            rr = radius_1d_rj_r(k)
            d_rj(it,itor%i_magne)                                       &
     &            =  (ten/three) * rr * sin(pi*(rr-r_ICB))
            d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
            d_rj(it,idpdr%i_current)                                    &
     &            = (ten / three) * (sin(pi*(rr-r_ICB))  &
     &                          + pi * rr * cos(pi*(rr-r_ICB)) )
          end do
        end if
!
      else
!
        if (js .gt. 0) then
          do k = 1, nlayer_CMB
            is = js + (k-1)*nidx_rj(2)
            rr = radius_1d_rj_r(k)
            d_rj(is,ipol%i_magne) =  (five / two) * rr**2               &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
            d_rj(is,idpdr%i_magne) = (five / two) * rr                  &
     &                       * (eight*r_CMB - dnine*rr) / (r_CMB+three)
            d_rj(is,itor%i_current) =  five*six * rr / (three +r_CMB)
          end do
        end if
!
        if (jt .gt. 0) then
          do k = 1, nlayer_CMB
            it = jt + (k-1)*nidx_rj(2)
            rr = radius_1d_rj_r(k)
!
            d_rj(it,itor%i_magne)                                       &
     &          =  (ten / three) * rr * sin(pi*rr/r_CMB)
            d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
            d_rj(it,idpdr%i_current)                                    &
     &          = (ten / three) * (sin(pi*rr/r_CMB)    &
     &                          + (pi/r_CMB) * rr * cos(pi*rr/r_CMB) )
          end do
        end if
!
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_noize_scalar_sph(is_fld)
!
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
!
!
      integer(kind = kint), intent(in) :: is_fld
      integer ( kind = kint) :: inod, j, k
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,is_fld) = zero
      end do
!$omp end parallel do
!
!
      if (idx_rj_degree_zero .gt. 0) then
        if ( iflag_4_ref_temp .eq. 100 ) then
          do k = 1, nidx_rj(1)
            inod = idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,is_fld) = reftemp_rj(k,0)
          end do
        end if
      end if
!
!
      do j = 1+idx_rj_degree_zero, nidx_rj(2)
        do k = nlayer_ICB+2, nlayer_CMB-2
          inod = j + (k-1)*nidx_rj(2)
!
          xr = two * radius_1d_rj_r(k)                                  &
     &       - (radius_1d_rj_r(nlayer_ICB+2)                            &
     &         +radius_1d_rj_r(nlayer_CMB-2) ) / shell
          d_rj(inod,is_fld) = (one-three*xr**2+three*xr**4-xr**6)       &
     &                       * 1.0d-4 * six / (sqrt(pi))
        end do
      end do
!
      end subroutine set_noize_scalar_sph
!
!-----------------------------------------------------------------------
!
      subroutine reduce_initial_magne_sph
!
      use m_spheric_parameter
      use m_sph_spectr_data
!
      integer(kind = kint) :: is
!
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,ipol%i_magne  ) = 1.0d-3 * d_rj(is,ipol%i_magne  )
        d_rj(is,ipol%i_magne+1) = 1.0d-3 * d_rj(is,ipol%i_magne+1)
        d_rj(is,ipol%i_magne+2) = 1.0d-3 * d_rj(is,ipol%i_magne+2)
        d_rj(is,ipol%i_current  ) = 1.0d-3 * d_rj(is,ipol%i_current  )
        d_rj(is,ipol%i_current+1) = 1.0d-3 * d_rj(is,ipol%i_current+1)
        d_rj(is,ipol%i_current+2) = 1.0d-3 * d_rj(is,ipol%i_current+2)
      end do
!$omp end parallel do
!
      end subroutine reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      end module set_initial_sph_dynamo

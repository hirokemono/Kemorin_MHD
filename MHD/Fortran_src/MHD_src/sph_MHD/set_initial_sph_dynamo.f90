!>@file   set_initial_sph_dynamo.f90
!!@brief  module set_initial_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_data_control(reftemp_rj, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module set_initial_sph_dynamo
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_sph_phys_address
      use t_spheric_rj_data
!
      implicit none
!
      private :: set_initial_temp_sph, set_initial_magne_sph
      private :: set_initial_light_sph, set_initial_velo_sph
      private :: set_ini_reference_temp_sph, set_all_part_temp_sph
      private :: set_noize_scalar_sph, reduce_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_control(reftemp_rj, rj_fld)
!
      use m_machine_parameter
      use m_initial_field_control
      use m_t_int_parameter
      use m_t_step_parameter
      use m_spheric_parameter
!
      use t_phys_data
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
      use initial_magne_dynamobench
      use initial_magne_dbench_qvc
!
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: isig
!
!
      iflag_initial_step = 0
      if (iflag_restart .eq. i_rst_by_file) then
        if(iflag_debug .gt. 0) write(*,*) 'read_alloc_sph_restart_data'
        call read_alloc_sph_restart_data(rj_fld)
!
!   for dynamo benchmark
!
      else if(iflag_restart .eq. i_rst_dbench0                          &
     &   .or. iflag_restart .eq. i_rst_dbench1                          &
     &   .or. iflag_restart .eq. i_rst_dbench2                          &
     &   .or. iflag_restart .eq. i_rst_dbench_qcv) then
        isig = 400
        call set_initial_velo_sph                                       &
     &     (nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        if(ipol%i_temp .gt. 0) then
          call set_ini_reference_temp_sph                               &
     &       (reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
          call set_initial_temp_sph(isig, sph_rj1%radius_1d_rj_r,       &
     &        rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_light .gt. 0) then
          call set_initial_light_sph(isig, ipol%i_light,                &
     &        sph_rj1%radius_1d_rj_r, reftemp_rj,                       &
     &        rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
        if(iflag_restart .eq. i_rst_dbench1) then
          if(ipol%i_magne .gt. 0) then
            call initial_b_dynamobench_1                                &
     &         (sph_rj1%radius_1d_rj_r, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench2) then
          if(ipol%i_magne .gt. 0) then
            call initial_b_dynamobench_2                                &
     &         (sph_rj1%radius_1d_rj_r, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        else if(iflag_restart .eq. i_rst_dbench_qcv) then
          if(ipol%i_magne .gt. 0) then
           call initial_b_dynamobench_qcv                               &
     &        (sph_rj1%radius_1d_rj_r, rj_fld%ntot_phys, rj_fld%d_fld)
          end if
        end if
!
!   set small seed magnetic field
!
      else if (iflag_restart .eq. i_rst_no_file) then
        if(ipol%i_temp .gt. 0)  then
          call set_noize_scalar_sph(ipol%i_temp, reftemp_rj,            &
     &        sph_rj1%radius_1d_rj_r, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_light .gt. 0) then
          call set_noize_scalar_sph(ipol%i_light, reftemp_rj,           &
     &        sph_rj1%radius_1d_rj_r, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_magne .gt. 0) then
          call set_initial_magne_sph(sph_rj1%radius_1d_rj_r,            &
     &        rj_fld%ntot_phys, rj_fld%d_fld)
          call reduce_initial_magne_sph                                 &
     &       (nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_restart .eq. i_rst_licv) then
        call set_ini_reference_temp_sph                                 &
     &     (reftemp_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        call set_all_part_temp_sph(sph_rj1%radius_1d_rj_r,              &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'init_output_sph_restart_file'
      call init_output_sph_restart_file(rj_fld)
!
      if (iflag_restart.ne.i_rst_by_file .and. i_step_init.eq.0) then
        if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
        call output_sph_restart_control(rj_fld)
      end if
!
      end subroutine sph_initial_data_control
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velo_sph                                   &
     &         (nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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
      subroutine set_ini_reference_temp_sph                             &
     &         (reftemp_rj, ntot_phys_rj, d_rj)
!
      use m_control_parameter
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, k, jj
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
!   set reference temperature (l = m = 0)
      if (sph_rj1%idx_rj_degree_zero .gt. 0) then
        if ( iflag_4_ref_temp .eq. id_sphere_ref_temp ) then
          do k = 1, nidx_rj(1)
            inod = sph_rj1%idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,ipol%i_temp) = reftemp_rj(k,0)
          end do
        else
          do k = 1, nlayer_ICB-1
            inod = local_sph_node_address(sph_rj1, k, jj)
            d_rj(inod,ipol%i_temp) = 1.0d0
          end do
          do k = nlayer_ICB, nlayer_CMB
            inod = sph_rj1%idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,ipol%i_temp)                                      &
     &           = (sph_rj1%ar_1d_rj(k,1) * 20.d0/13.0d0 - 1.0d0 )      &
     &            * 7.0d0 / 13.0d0
          end do
        end if
      end if
!
!    Center
      if(sph_rj1%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj1, 0, 0)
        inod = local_sph_node_address(sph_rj1, 1, jj)
        d_rj(sph_rj1%inod_rj_center,ipol%i_temp)                        &
     &       = d_rj(inod,ipol%i_temp)
      end if
!
      end subroutine set_ini_reference_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_all_part_temp_sph                                  &
     &          (radius_1d_rj_r, ntot_phys_rj, d_rj)
!
      use m_control_parameter
      use m_spheric_parameter
!
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, j, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      do j = 1, nidx_rj(2)
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = j + (k-1)*nidx_rj(2)
!
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * 0.1d0 * six / (sqrt(pi))
        end do
      end do
!
!    Center
      if(sph_rj1%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj1, 0, 0)
        inod = local_sph_node_address(sph_rj1, 1, jj)
        d_rj(sph_rj1%inod_rj_center,ipol%i_temp)                        &
     &       = d_rj(inod,ipol%i_temp)
      end if
!
      end subroutine set_all_part_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp_sph                                   &
     &         (isig, radius_1d_rj_r, ntot_phys_rj, d_rj)
!
      use m_control_parameter
      use m_spheric_parameter
!
      integer ( kind = kint), intent(in) :: isig
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = 4) ::  m
      integer(kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
      m = int( mod(isig,ikilo) / icent )
      jj = find_local_sph_address(sph_rj1, m, m)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
!
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(sph_rj1%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj1, 0, 0)
        inod = local_sph_node_address(sph_rj1, 1, jj)
        d_rj(sph_rj1%inod_rj_center,ipol%i_temp)                        &
     &       = d_rj(inod,ipol%i_temp)
      end if
!
      end subroutine set_initial_temp_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_light_sph(isig, is_fld, radius_1d_rj_r,    &
     &          reftemp_rj, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
!
      integer ( kind = kint), intent(in) :: isig, is_fld
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = 4) :: m
      integer ( kind = kint) :: inod, k, jj
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
      if (sph_rj1%idx_rj_degree_zero .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = sph_rj1%idx_rj_degree_zero + (k-1)*nidx_rj(2)
          d_rj(inod,is_fld) = reftemp_rj(k,0)
        end do
      end if
!
!
      m = int( mod(isig,ikilo) / icent )
      jj = find_local_sph_address(sph_rj1, m, m)
!
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
          d_rj(inod,is_fld) = (one-three*xr**2+three*xr**4-xr**6)      &
     &                            * 0.1d0 * three / (sqrt(two*pi))
        end do
      end if
!
!    Center
      if(sph_rj1%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj1, 0, 0)
        inod = local_sph_node_address(sph_rj1, 1, jj)
        d_rj(sph_rj1%inod_rj_center,is_fld) = d_rj(inod,is_fld)
      end if
!
      end subroutine set_initial_light_sph
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph                                  &
     &         (radius_1d_rj_r, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt
!
!
      js = find_local_sph_address(sph_rj1, 1,0)
      jt = find_local_sph_address(sph_rj1, 2,0)
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
      if (sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
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
      subroutine set_noize_scalar_sph                                   &
     &         (is_fld, reftemp_rj, radius_1d_rj_r, ntot_phys_rj, d_rj)
!
      use m_spheric_parameter
      use m_control_parameter
!
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      integer(kind = kint), intent(in) :: ntot_phys_rj
      real(kind=kreal), intent(in) :: reftemp_rj(nidx_rj(1),0:1)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer ( kind = kint) :: inod, j, k, jj
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
      if (sph_rj1%idx_rj_degree_zero .gt. 0) then
        if ( iflag_4_ref_temp .eq. id_sphere_ref_temp ) then
          do k = 1, nidx_rj(1)
            inod = sph_rj1%idx_rj_degree_zero + (k-1)*nidx_rj(2)
            d_rj(inod,is_fld) = reftemp_rj(k,0)
          end do
        end if
      end if
!
!
      do j = 1+sph_rj1%idx_rj_degree_zero, nidx_rj(2)
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
!    Center
      if(sph_rj1%inod_rj_center .gt. 0) then
        jj = find_local_sph_address(sph_rj1, 0, 0)
        inod = local_sph_node_address(sph_rj1, 1, jj)
        d_rj(sph_rj1%inod_rj_center,is_fld) = d_rj(inod,is_fld)
      end if
!
      end subroutine set_noize_scalar_sph
!
!-----------------------------------------------------------------------
!
      subroutine reduce_initial_magne_sph(nnod_rj, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
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

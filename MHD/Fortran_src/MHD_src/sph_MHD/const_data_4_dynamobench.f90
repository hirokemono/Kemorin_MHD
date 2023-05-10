!>@file   const_data_4_dynamobench.f90
!!@brief  module const_data_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine const_dynamobench_data                               &
!!     &         (time_d, sph_params, sph_rj, sph_MHD_bc, trans_p, ipol,&
!!     &          rj_fld, pwr, cdat, bench)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
!
      module const_data_4_dynamobench
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: mid_eq_transfer_dynamobench
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_mid_equator_fld_dbench(ipol, bench)
!
      use t_phys_address
      use t_field_4_dynamobench
!
      type(phys_address), intent(in) :: ipol
!
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: i_fld
!
!
      i_fld = 1
      if(ipol%base%i_velo .gt. 0) then
        bench%iphys_circle%base%i_velo = i_fld
        i_fld = i_fld + n_vector
      end if
      if(ipol%base%i_magne .gt. 0) then
        bench%iphys_circle%base%i_magne = i_fld
        i_fld = i_fld + n_vector
      end if
      if(ipol%base%i_temp .gt. 0) then
        bench%iphys_circle%base%i_temp = i_fld
        i_fld = i_fld + n_scalar
      end if
      if(ipol%base%i_light .gt. 0) then
        bench%iphys_circle%base%i_light = i_fld
        i_fld = i_fld + n_scalar
      end if
!
      end subroutine count_mid_equator_fld_dbench
!
! ----------------------------------------------------------------------
!
      subroutine init_address_dbench_trans(rj_fld, ipol, bench)
!
      use t_phys_data
      use t_phys_address
      use t_field_4_dynamobench
!
      use set_address_circle_trans
!
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
!
      type(dynamobench_monitor), intent(inout) :: bench
!
!
      bench%ncomp_sph_trans_meq = 0
      bench%nvec_sph_trans_meq =  0
      bench%nscl_sph_trans_meq =  0
      call set_addresses_circle_trans                                   &
     &   (rj_fld, ipol, bench%iphys_circle, bench%trns_dbench,          &
     &    bench%ncomp_sph_trans_meq, bench%nvec_sph_trans_meq,          &
     &    bench%nscl_sph_trans_meq)
!
!      if(my_rank .ne. 0) return
!      write(*,*) 'Velocity',     ipol%base%i_velo,                     &
!     &              bench%iphys_circle%base%i_velo,                    &
!     &        bench%trns_dbench%b_trns%base%i_velo
!      write(*,*) 'Magnetic',     ipol%base%i_magne,                    &
!     &              bench%iphys_circle%base%i_magne,                   &
!     &        bench%trns_dbench%b_trns%base%i_magne
!      write(*,*) 'Temperature',  ipol%base%i_temp,                     &
!     &              bench%iphys_circle%base%i_temp,                    &
!     &        bench%trns_dbench%b_trns%base%i_temp
!      write(*,*) 'Composition',  ipol%base%i_light,                    &
!     &              bench%iphys_circle%base%i_light,                   &
!     &        bench%trns_dbench%b_trns%base%i_light
!
      end subroutine init_address_dbench_trans
!
! ----------------------------------------------------------------------
!
      subroutine const_dynamobench_data                                 &
     &         (time_d, sph_params, sph_rj, sph_MHD_bc, trans_p, ipol,  &
     &          rj_fld, pwr, cdat, bench)
!
      use field_at_mid_equator
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use field_at_mid_equator
      use t_boundary_data_sph_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_work_4_sph_trans
!
      use calypso_mpi
      use calypso_mpi_real
      use cal_rms_fields_by_sph
      use global_field_4_dynamobench
      use transfer_to_long_integers
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: irank_copy
!
!
      if(bench%iflag_dynamobench .le. 0) return
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench(time_d%time, trans_p%iflag_FFT,  &
     &    sph_rj, rj_fld, ipol, cdat, bench)
!
      if(bench%ipwr_ocore .gt. 0) then
        irank_copy = pwr%v_spectr(bench%ipwr_ocore)%irank_m
        if(my_rank .eq. irank_copy)  then
          call copy_kin_energy_4_dbench(bench%ipwr_ocore, pwr,          &
     &                                  bench%KE_bench)
          call copy_mag_energy_4_dbench(bench%ipwr_ocore, pwr,          &
     &                                  bench%ME_bench)
        end if
        call calypso_mpi_bcast_real(bench%KE_bench, cast_long(ithree),  &
     &                              irank_copy)
        call calypso_mpi_bcast_real(bench%ME_bench, cast_long(ithree),  &
     &                              irank_copy)
      end if
!
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation(sph_rj%idx_rj_degree_one,         &
     &      sph_rj%nidx_rj, sph_params%nlayer_ICB, sph_rj%ar_1d_rj,     &
     &      ipol%base%i_velo, rj_fld%n_point, rj_fld%ntot_phys,         &
     &      rj_fld%d_fld, bench%rotate_icore)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        if(bench%ipwr_icore .gt. 0) then
          irank_copy = pwr%v_spectr(bench%ipwr_ocore)%irank_m
          if(my_rank .eq. irank_copy)  then
            call copy_mag_energy_4_dbench(bench%ipwr_icore, pwr,        &
     &                                    bench%mene_icore)
          end if
          call calypso_mpi_bcast_real(bench%mene_icore,                 &
     &                                cast_long(ithree), irank_copy)
        end if
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center       &
     &   .and. sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic)   &
     & then
        call pick_mag_torque_inner_core                                 &
     &     (sph_rj%idx_rj_degree_one,  sph_rj%nidx_rj,                  &
     &      sph_params%nlayer_ICB, sph_rj%radius_1d_rj_r,               &
     &      ipol%forces%i_lorentz, rj_fld%n_point, rj_fld%ntot_phys,    &
     &      rj_fld%d_fld, bench%m_torque_icore)
      end if
!
      end subroutine const_dynamobench_data
!
! ----------------------------------------------------------------------
!
      subroutine mid_eq_transfer_dynamobench                            &
     &         (time, iflag_FFT, sph_rj, rj_fld, ipol, cdat, bench)
!
      use calypso_mpi
      use t_field_on_circle
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_circle_transform
      use t_field_4_dynamobench
!
      use field_at_mid_equator
      use circle_bwd_transfer_rj
!
      real(kind=kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer :: i, j, n, ifld
!
!    spherical transfer
!
      call sph_transfer_on_circle(iflag_FFT, sph_rj, rj_fld, cdat)
!
!
      allocate(cdat%leg_crc%d_circ_gl(-cdat%circ_spec%ltr_circle:cdat%circ_spec%ltr_circle,    &
     &                   cdat%d_circle%ntot_phys))
      allocate(cdat%leg_crc%d_circ_lc(-cdat%circ_spec%ltr_circle:cdat%circ_spec%ltr_circle,    &
     &                   cdat%d_circle%ntot_phys))
!
      allocate(cdat%leg_crc%vrtm_mag(0:cdat%circle%mphi_circle,cdat%d_circle%ntot_phys_viz))
      allocate(cdat%leg_crc%vrtm_phase(0:cdat%circle%mphi_circle, cdat%d_circle%ntot_phys_viz))
      allocate(cdat%leg_crc%v_rtp_circle(cdat%circle%mphi_circle, cdat%d_circle%ntot_phys_viz))
!
      call dbench_leg_bwd_trans_rj(iflag_FFT, sph_rj, rj_fld, ipol,                &
     &    bench%trns_dbench%b_trns, cdat%circle, cdat%circ_spec,         &
     &    cdat%d_circle, cdat%leg_crc%P_circ, cdat%leg_crc%dPdt_circ,   &
     &    cdat%leg_crc%d_circ_gl, cdat%leg_crc%d_circ_lc, cdat%leg_crc%vrtm_mag, &
     &    cdat%leg_crc%vrtm_phase, cdat%leg_crc%v_rtp_circle, &
     &    cdat%WK_circle_fft)
!
!      if(my_rank .eq. 0) then
!        i = bench%trns_dbench%b_trns%base%i_velo
!        write(60,*) 'j, velo_new', ipol%base%i_velo, i
!        do j = -cdat%circ_spec%ltr_circle, cdat%circ_spec%ltr_circle
!          write(60,*) j, cdat%leg_crc%d_circ_gl(j,i:i+2)
!        end do
!        i = bench%trns_dbench%b_trns%base%i_magne
!        write(60,*) 'j, magne_new', ipol%base%i_magne, i
!        do j = -cdat%circ_spec%ltr_circle, cdat%circ_spec%ltr_circle
!          write(60,*) j, cdat%leg_crc%d_circ_gl(j,i:i+2)
!        end do
!!
!        i = bench%trns_dbench%b_trns%base%i_temp
!        write(60,*) 'j, temp_new', ipol%base%i_temp, i
!        do j = -cdat%circ_spec%ltr_circle, cdat%circ_spec%ltr_circle
!        write(60,*) j, cdat%leg_crc%d_circ_gl(j,i)
!        end do
!      end if
!
      if(my_rank .eq. 0) then
        i = bench%trns_dbench%b_trns%base%i_velo
        write(60,*) 'j, velo_new', ipol%base%i_velo, i
        do j = 1, cdat%circle%mphi_circle
          write(60,*) j, cdat%leg_crc%v_rtp_circle(j,i:i+2)
        end do
        i = bench%trns_dbench%b_trns%base%i_magne
        write(60,*) 'j, magne_new', ipol%base%i_magne, i
        do j = 1, cdat%circle%mphi_circle
          write(60,*) j, cdat%leg_crc%v_rtp_circle(j,i:i+2)
        end do
!!
        i = bench%trns_dbench%b_trns%base%i_temp
        write(60,*) 'j, temp_new', ipol%base%i_temp, i
        do j = 1, cdat%circle%mphi_circle
        write(60,*) j, cdat%leg_crc%v_rtp_circle(j,i)
        end do
!
        do ifld = 1, cdat%d_circle%num_phys_viz
          i = cdat%d_circle%istack_component(ifld-1)
          n = cdat%d_circle%istack_component(ifld) - i
          write(61,*) 'j', trim(cdat%d_circle%phys_name(ifld)), ifld, i
          do j = 1, cdat%circle%mphi_circle
            write(61,*) j, cdat%d_circle%d_fld(j,i+1:i+n)
          end do
        end do
      end if
!
      deallocate(cdat%leg_crc%vrtm_mag, cdat%leg_crc%vrtm_phase, cdat%leg_crc%v_rtp_circle)
      deallocate(cdat%leg_crc%d_circ_gl, cdat%leg_crc%d_circ_lc)
!
      if(my_rank .gt. 0) return
!
!
!
!   Evaluate drift frequencty by velocity 
!
      call cal_drift_by_v44(time, cdat%circle, bench%ibench_velo,       &
     &    bench%t_prev, bench%phase_vm4, bench%phase_vm4_prev,          &
     &    bench%omega_vm4)
!
!   find local point for dynamobench
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_field_4_dynamobench'
      call cal_field_4_dynamobench                                      &
     &   (time, bench%t_prev, cdat%circle, cdat%d_circle,               &
     &    bench%ibench_velo, bench%phi_zero, bench%phi_prev,            &
     &    bench%phase_vr, bench%ave_phase_vr, bench%d_zero)
      bench%t_prev = time
!
      end subroutine mid_eq_transfer_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine sph_forward_trans_on_circles                           &
     &         (iflag_FFT, sph_rj, rj_fld, nod_fld,                     &
     &          circle, circ_spec, d_circle, leg_crc, WK_circle_fft)
!
      use calypso_mpi
      use t_field_on_circle
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_circle_transform
      use t_field_4_dynamobench
!
      use calypso_mpi_real
      use transfer_to_long_integers
      use circle_bwd_transfer_rj
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_data), intent(in) :: nod_fld
      type(fields_on_circle), intent(in) :: circle
      type(circle_transform_spetr), intent(in) :: circ_spec
      type(phys_data), intent(in) :: d_circle
!
      type(leg_circle), intent(inout) :: leg_crc
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
!
      allocate(leg_crc%d_circ_gl(-circ_spec%ltr_circle:circ_spec%ltr_circle,    &
     &                   nod_fld%ntot_phys_viz))
      allocate(leg_crc%d_circ_lc(-circ_spec%ltr_circle:circ_spec%ltr_circle,    &
     &                   nod_fld%ntot_phys_viz))
!
      allocate(leg_crc%vrtm_mag(0:circle%mphi_circle,nod_fld%ntot_phys_viz))
      allocate(leg_crc%vrtm_phase(0:circle%mphi_circle, nod_fld%ntot_phys_viz))
      allocate(leg_crc%v_rtp_circle(circle%mphi_circle, nod_fld%ntot_phys_viz))
!
      call circle_leg_bwd_trans_rj                                      &
     &   (iflag_FFT, sph_rj, rj_fld, nod_fld, leg_crc%ipol_circle_trns, circle,    &
     &    circ_spec, d_circle, leg_crc%P_circ, leg_crc%dPdt_circ,       &
     &    leg_crc%d_circ_gl, leg_crc%d_circ_lc, leg_crc%vrtm_mag, leg_crc%vrtm_phase, leg_crc%v_rtp_circle, &
     &    WK_circle_fft)
!
      deallocate(leg_crc%vrtm_mag, leg_crc%vrtm_phase, leg_crc%v_rtp_circle)
      deallocate(leg_crc%d_circ_gl, leg_crc%d_circ_lc)
!
      end subroutine sph_forward_trans_on_circles
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench

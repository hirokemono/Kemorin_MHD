!m_field_4_dynamobench.f90
!      module m_field_4_dynamobench
!
!      Programmed by H. Matsui on June., 2011
!
!      subroutine open_dynamobench_monitor_file
!      subroutine output_field_4_dynamobench(i_step, time)
!      subroutine close_dynamobench_monitor_file
!
      module m_field_4_dynamobench
!
      use m_precision
      use m_constants
      use m_parallel_var_dof
      use m_file_control_parameter
!
      implicit none
!
      integer(kind = kint), parameter :: ibench_temp =  1
      integer(kind = kint), parameter :: ibench_velo =  2
      integer(kind = kint), parameter :: ibench_magne = 5
!
      real(kind = kreal) :: KE_bench(3)
      real(kind = kreal) :: ME_bench(3)
!
      real(kind = kreal) :: t_prev = zero
      real(kind = kreal) :: phi_zero(4) = (/zero,zero,zero,zero/)
      real(kind = kreal) :: phi_prev(4) = (/zero,zero,zero,zero/)
      real(kind = kreal) :: drift(0:4)
      real(kind = kreal) :: mene_icore(3)
      real(kind = kreal) :: rotate_icore(-1:1)
      real(kind = kreal) :: m_torque_icore(-1:1)
!
      real(kind = kreal) :: phase_vm4(2)      = (/zero,zero/)
      real(kind = kreal) :: phase_vm4_prev(2) = (/zero,zero/)
      real(kind = kreal) :: omega_vm4(2)      = (/zero,zero/)
!
      real(kind = kreal) :: d_zero(0:4,7)
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_dynamobench_monitor_file
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
!
      if(my_rank .ne. 0) return
!
      open(id_dynamobench, file=dynamobench_field_name)
!
      write(id_dynamobench,'(a)', advance='NO') 't_step, time'
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     ', KE_pol, KE_tor, KE_total'
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     ', ME_pol, ME_tor, ME_total'
      end if
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     ', ME_pol_icore, ME_tor_icore, ME_total_icore'
      end if
!
      if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') ', omega_ic_z'
      end if
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center                     &
     &  .and. iflag_icb_velocity .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') ', MAG_torque_ic_z'
      end if
!
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     ', phi_1, phi_2, phi_3, phi_4, omega_vp44, omega_vt54'
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO') ', B_theta'
      end if
!
      write(id_dynamobench,'(a)')  ', v_phi, temp'
!
      end subroutine open_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine output_field_4_dynamobench(i_step, time)
!
      use m_control_params_sph_MHD
      use m_sph_phys_address
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      if(my_rank .ne. 0) return
!
      write(id_dynamobench,'(i10,1pE25.15e3)', advance='NO')            &
     &     i_step, time
      write(id_dynamobench,'(1p3E25.15e3)', advance='NO') KE_bench(1:3)
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     ME_bench(1:3)
      end if
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     mene_icore(1:3)
      end if
!
      if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     rotate_icore(0)
      end if
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center                     &
     &   .and. iflag_icb_velocity .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     m_torque_icore(0)
      end if
!
      write(id_dynamobench,'(1p4E25.15e3)', advance='NO') phi_zero(1:4)
      write(id_dynamobench,'(1p2E25.15e3)', advance='NO')               &
     &      omega_vm4(1:2)
!
      if(irtp%i_magne .gt. 0) then
        write(id_dynamobench,'(1p2E25.15e3)', advance='NO')             &
     &      d_zero(0,ibench_magne+1)
      end if
!
      write(id_dynamobench,'(1p2E25.15e3)')                             &
     &     d_zero(0,ibench_velo+2), d_zero(0,ibench_temp)
!
      end subroutine output_field_4_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine close_dynamobench_monitor_file
!
!
      if(my_rank .ne. 0) close(id_dynamobench)
!
      end subroutine close_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      end module m_field_4_dynamobench

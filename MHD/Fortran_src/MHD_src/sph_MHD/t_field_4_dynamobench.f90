!>@file   t_field_4_dynamobench.f90
!!@brief  module t_field_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine open_dynamobench_monitor_file                        &
!!     &         (sph_bc_U, sph_bc_B, ipol)
!!      subroutine output_field_4_dynamobench                           &
!!     &          (i_step, time, sph_bc_U, sph_bc_B, ipol, bench)
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(phys_address), intent(in) :: ipol
!!        type(dynamobench_monitor), intent(in) :: bench
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module t_field_4_dynamobench
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_phys_address
      use t_boundary_data_sph_MHD
!
      implicit none
!
!>      file ID for benchmark output file
      integer(kind=kint), parameter :: id_dynamobench = 41
!>      file name for benchmark output file
      character(len=kchara), parameter                                  &
     &      :: dynamobench_field_name = 'dynamobench_field.dat'
!
!
      type dynamobench_monitor
!>        temperature address for spherical transform at equator
        integer(kind = kint) :: ibench_temp =  1
!>        velocity address for spherical transform at equator
        integer(kind = kint) :: ibench_velo =  2
!>        magnetic field address for spherical transform at equator
        integer(kind = kint) :: ibench_magne = 5
!
!>        average kinetic energy (poloidal, toroidal, total)
        real(kind = kreal) :: KE_bench(3)
!>        average magnetic energy (poloidal, toroidal, total)
        real(kind = kreal) :: ME_bench(3)
!
!>        time for previus monitoring of omega
        real(kind = kreal) :: t_prev = zero
!>        longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
        real(kind = kreal) :: phi_zero(4) = (/zero,zero,zero,zero/)
!>        longitude where @f$ u_[r} = 0, \partial_{\phi} u_{r} > 0 @f$
!!        at previous monitoring
        real(kind = kreal) :: phi_prev(4) = (/zero,zero,zero,zero/)
!>        mangetic energy in inner core
        real(kind = kreal) :: mene_icore(3)
!>        rotation rate for inner core
        real(kind = kreal) :: rotate_icore(-1:1)
!>        magnetic torque for inner core
        real(kind = kreal) :: m_torque_icore(-1:1)
!
!>        phase of by @f$ V_{S4}^{4} @f$
        real(kind = kreal) :: phase_vm4(2)      = (/zero,zero/)
!>        phase of by @f$ V_{S4}^{4} @f$
!!        at previous monitoring
        real(kind = kreal) :: phase_vm4_prev(2) = (/zero,zero/)
!>        drift frequency obtained by @f$ V_{S4}^{4} @f$
        real(kind = kreal) :: omega_vm4(2)      = (/zero,zero/)
!
!>        local point data
        real(kind = kreal) :: d_zero(0:4,7)
      end type dynamobench_monitor
!
      private :: id_dynamobench, dynamobench_field_name
      private :: open_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine open_dynamobench_monitor_file                          &
     &         (sph_bc_U, sph_bc_B, ipol)
!
      type(sph_boundary_type), intent(in) :: sph_bc_U, sph_bc_B
      type(phys_address), intent(in) :: ipol
!
!
      open(id_dynamobench, file=dynamobench_field_name,                 &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
  99  continue
      open(id_dynamobench, file=dynamobench_field_name)
!
      write(id_dynamobench,'(a)', advance='NO') 't_step    time    '
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'KE_pol    KE_tor    KE_total    '
!
      if(ipol%base%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     'ME_pol    ME_tor    ME_total    '
      end if
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(a)', advance='NO')                       &
     &     'ME_pol_icore    ME_tor_icore    ME_total_icore    '
      end if
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') 'omega_ic_z    '
      end if
!
      write(*,*) 'sph_bc_U%iflag_icb', sph_bc_U%iflag_icb
      if(sph_bc_B%iflag_icb .eq. iflag_sph_fill_center                  &
     &  .and. sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(a)', advance='NO') 'MAG_torque_ic_z    '
      end if
!
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'phi_1    phi_2    phi_3    phi_4    '
      write(id_dynamobench,'(a)', advance='NO')                         &
     &     'omega_vp44    omega_vt54    '
!
      if(ipol%base%i_magne .gt. 0) then
        write(id_dynamobench,'(a)', advance='NO') 'B_theta    '
      end if
!
      write(id_dynamobench,'(a)')  'v_phi    temp'
!
      end subroutine open_dynamobench_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine output_field_4_dynamobench                             &
     &          (i_step, time, sph_MHD_bc, ipol, bench)
!
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(dynamobench_monitor), intent(in) :: bench
!
!
      if(my_rank .ne. 0) return
!
      call open_dynamobench_monitor_file                                &
     &   (sph_MHD_bc%sph_bc_U, sph_MHD_bc%sph_bc_B, ipol)
!
      write(id_dynamobench,'(i15,1pE25.15e3)', advance='NO')            &
     &     i_step, time
      write(id_dynamobench,'(1p3E25.15e3)', advance='NO')               &
     &     bench%KE_bench(1:3)
!
      if(ipol%base%i_magne .gt. 0) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     bench%ME_bench(1:3)
      end if
!
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        write(id_dynamobench,'(1p3E25.15e3)', advance='NO')             &
     &     bench%mene_icore(1:3)
      end if
!
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     bench%rotate_icore(0)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center       &
     &   .and. sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic)   &
     & then
        write(id_dynamobench,'(1pE25.15e3)', advance='NO')              &
     &     bench%m_torque_icore(0)
      end if
!
      write(id_dynamobench,'(1p4E25.15e3)', advance='NO')               &
     &      bench%phi_zero(1:4)
      write(id_dynamobench,'(1p2E25.15e3)', advance='NO')               &
     &      bench%omega_vm4(1:2)
!
      if(ipol%base%i_magne .gt. 0) then
        write(id_dynamobench,'(1p2E25.15e3)', advance='NO')             &
     &      bench%d_zero(0,bench%ibench_magne+1)
      end if
!
      write(id_dynamobench,'(1p2E25.15e3)')                             &
     &     bench%d_zero(0,bench%ibench_velo+2),                         &
     &     bench%d_zero(0,bench%ibench_temp)
!
      close(id_dynamobench)
!
      end subroutine output_field_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module t_field_4_dynamobench

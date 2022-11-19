!>@file   write_picked_sph_spectr.f90
!!@brief  module write_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_each_picked_specr_file                         &
!!     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!      subroutine open_eack_picked_spectr                              &
!!     &         (id_pick, nlayer_ICB, nlayer_CMB, picked, l, m)
!!        integer(kind = kint), intent(in) :: id_pick, l, m
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!@endverbatim
!!
      module write_picked_sph_spectr
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_each_picked_specr_file                           &
     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!
      use pickup_sph_spectr_data
      use sph_monitor_data_text
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint), parameter :: id_pick = 17
      integer(kind = kint) :: inum, knum
      integer(kind = kint_gl) :: num
!
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call open_eack_picked_spectr(id_pick,                           &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      izero, izero)
        call pick_center_spectrum_monitor                               &
     &     (rj_fld, picked, picked%ntot_comp_rj, d_rj_out)
        write(id_pick) picked_each_mode_data_text                       &
     &               (time_d%i_time_step, time_d%time, zero, izero,     &
     &                izero, izero, picked%ntot_comp_rj, d_rj_out)
        close(id_pick)
      end if
!
      do inum = 1, picked%num_sph_mode_lc
        call open_eack_picked_spectr(id_pick,                           &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,         &
     &      picked%idx_out(inum,1), picked%idx_out(inum,2))
        do knum = 1, picked%num_layer
          call pick_single_sph_spec_4_monitor(inum, knum,               &
     &        sph_rj, rj_fld, picked, picked%ntot_comp_rj, d_rj_out)
!
          write(id_pick) picked_each_mode_data_text                     &
     &                             (time_d%i_time_step, time_d%time,    &
     &                              picked%radius_gl(knum),             &
     &                              picked%id_radius(knum),             &
     &                              picked%idx_out(inum,1),             &
     &                              picked%idx_out(inum,2),             &
     &                              picked%ntot_comp_rj, d_rj_out)
        end do
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine write_each_picked_specr_file
!
! -----------------------------------------------------------------------
!
      subroutine open_eack_picked_spectr                                &
     &         (id_pick, nlayer_ICB, nlayer_CMB, picked, l, m)
!
      use set_parallel_file_name
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_pick, l, m
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: picked
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: mm, i
!
      mm = abs(m)
      write(fname_tmp,'(a,a2)') trim(picked%file_prefix), '_l'
      call add_index_after_name(l, fname_tmp, file_name)
      write(fname_tmp,'(a,a2)') trim(file_name), '_m'
      call add_index_after_name(mm, fname_tmp, file_name)
      if(m .lt. 0) then
        write(fname_tmp,'(a,a1)') trim(file_name), 's'
      else
        write(fname_tmp,'(a,a1)') trim(file_name), 'c'
      end if
      file_name = add_dat_extension(fname_tmp)
      open(id_pick, file=file_name, status='old', position='append',    &
     &     form='unformatted', ACCESS='stream', err = 99)
      return
!
!
   99 continue
      open(id_pick, file=file_name, status='replace',                   &
     &     form='unformatted', ACCESS='stream')
!
      call write_each_pick_sph_file_header                              &
     &    (id_pick, nlayer_ICB, nlayer_CMB, picked)
!
      end subroutine open_eack_picked_spectr
!
! -----------------------------------------------------------------------
!
      end module write_picked_sph_spectr

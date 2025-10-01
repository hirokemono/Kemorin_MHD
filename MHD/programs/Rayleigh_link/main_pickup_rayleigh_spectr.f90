!>@file   main_pickup_rayleigh_spectr.f90
!!@brief  program pickup_rayleigh_spectr
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program for assemble spectr data
!
      program pickup_rayleigh_spectr
!
      use m_precision
!
      use calypso_mpi
      use t_field_data_IO
      use t_ctl_pick_rayleigh_spectr
      use t_picked_rayleigh_parameter
      use t_rayleigh_restart_IO
      use t_picked_rayleigh_spectr
      use t_ctl_data_gauss_coefs
!
      use rayleigh_restart_IO
      use MPI_read_rayleigh_restart
!
      implicit none
!
      integer(kind = kint), parameter  :: id_control = 11
      character(len = kchara), parameter                                &
     &                        :: control_name = 'control_pick_rayleigh'
!
      type(pick_rayleigh_spectr_control) :: pick_ctl_s
      type(rayleigh_restart) :: ra_rst_s
      type(picked_rayleigh_parameter) :: pick_ra_param_s
      type(picked_rayleigh_spectr) :: ra_picked_s
      type(field_IO) :: fld_IO_s
!
      integer(kind = kint) :: ierr = 0
      type(buffer_for_control) :: c_buf1
!
!
      call calypso_MPI_init
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_rayleigh_pick_mode_ctl                                &
     &     (id_control, control_name, pick_ctl_s, c_buf1)
      end if
      call bcast_pick_rayleigh_ctl(pick_ctl_s)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(pick_ctl_s%i_pick_rayleigh_spectr,       &
     &                             'control file is broken')
      end if
!
      call init_picked_rayleigh_param(pick_ctl_s, pick_ra_param_s)
      ra_rst_s%i_version = pick_ctl_s%Rayleigh_version_ctl%intvalue(1)
      call dealloc_pick_rayleigh_spectr(pick_ctl_s)
!
      if(my_rank .eq. 0) then
        call sel_read_rayleigh_rst_params                               &
     &     (pick_ra_param_s%Rayleigh_rst_dir, pick_ra_param_s%i_step,   &
     &      ra_rst_s)
      end if
      call bcast_rayleigh_restart_param(ra_rst_s)
!      call check_rayleigh_rst_params(6, ra_rst_s)
!
      call init_rayleigh_restart_input(ra_rst_s%i_version,              &
     &    pick_ra_param_s%Rayleigh_rst_dir, pick_ra_param_s%i_step,     &
     &    fld_IO_s)
!      call check_field_name_4_IO(6, fld_IO_s)
!
      call const_picked_rayleigh_prev_vec                               &
     &   (ra_rst_s, fld_IO_s, pick_ra_param_s, ra_picked_s)
!
      call dealloc_phys_data_IO(fld_IO_s)
      call dealloc_phys_name_IO(fld_IO_s)
!
      call dealloc_picked_rayleigh_param(pick_ra_param_s)
!
      call calypso_MPI_finalize
!
      stop '***** program finished *****'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_pick_rayleigh_ctl(pick_ctl)
!
      use t_ctl_pick_rayleigh_spectr
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pick_rayleigh_spectr_control), intent(inout) :: pick_ctl
!
!
      call bcast_ctl_type_c1(pick_ctl%picked_data_file_name)
      call bcast_ctl_type_c1(pick_ctl%Rayleigh_rst_dir_ctl)
      call bcast_ctl_type_i2(pick_ctl%Rayleigh_version_ctl)
      call bcast_ctl_type_i1(pick_ctl%Rayleigh_step_ctl)
      call bcast_ctl_array_i2(pick_ctl%idx_rayleigh_ctl)
      call calypso_mpi_bcast_one_int                                    &
     &    (pick_ctl%i_pick_rayleigh_spectr, 0)
!
      end subroutine bcast_pick_rayleigh_ctl
!
! -----------------------------------------------------------------------
!
      subroutine const_picked_rayleigh_prev_vec                         &
     &         (ra_rst, fld_IO, pick_ra_param, ra_picked)
!
      use m_calypso_mpi_IO
      use t_field_data_IO
      use t_rayleigh_restart_IO
      use t_picked_rayleigh_parameter
      use t_picked_rayleigh_spectr
!
      use m_base_field_labels
      use m_explicit_term_labels
!
      use rayleigh_restart_IO
      use MPI_read_rayleigh_restart
!
      type(rayleigh_restart), intent(in) :: ra_rst
      type(field_IO), intent(in)  :: fld_IO
      type(picked_rayleigh_parameter), intent(in) :: pick_ra_param
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: iflag_ncomp
      character(len = kchara) :: rayleigh_field_name(2)
!
      integer ::  id_mpi_pol
      integer ::  id_mpi_tor
!
!
      call init_each_rayleigh_spectr                                    &
     &   (ra_rst%nri_org, ra_rst%r_org, ra_picked)
      do i = 1, fld_IO%num_field_IO
        call set_rayleigh_rst_file_name(ra_rst%i_version,               &
     &      pick_ra_param%Rayleigh_rst_dir, pick_ra_param%i_step,       &
     &      fld_IO%fld_name(i), iflag_ncomp, rayleigh_field_name)
!
        if(iflag_ncomp .eq. 1) then
          write(*,*) i, trim(fld_IO%fld_name(i)), ':  ',                &
     &              trim(rayleigh_field_name(1))
          call calypso_mpi_read_file_open                               &
     &       (rayleigh_field_name(1), id_mpi_pol)
          do j = 1, pick_ra_param%num_modes
            if(my_rank .ne. mod(j-1,nprocs)) cycle
!
            call read_each_mode_from_rayleigh(id_mpi_pol, ra_rst,       &
     &          pick_ra_param%l_pick(j), pick_ra_param%m_pick(j),       &
     &          ra_picked%poloidal_re, ra_picked%poloidal_im)
!
            call set_picked_rayleigh_file_name                          &
     &         (pick_ra_param%picked_prefix, fld_IO%fld_name(i),        &
     &          pick_ra_param%l_pick(j), pick_ra_param%m_pick(j),       &
     &          pick_ra_param%i_step, ra_picked)
            if(     fld_IO%fld_name(i) .eq. temperature%name            &
     &         .or. fld_IO%fld_name(i) .eq. pressure%name) then
              call write_picked_rayleigh_scalar(ra_picked)
            else if(fld_IO%fld_name(i) .eq. previous_heat%name) then
              call write_picked_rayleigh_prev_scl(ra_picked)
            end if
          end do
          call calypso_close_mpi_file(id_mpi_pol)
!
        else if(iflag_ncomp .eq. 2) then
          write(*,*) i, trim(fld_IO%fld_name(i)), ':  ',                &
     &              trim(rayleigh_field_name(1)), ':  ',                &
     &              trim(rayleigh_field_name(2))
          call calypso_mpi_read_file_open                               &
     &       (rayleigh_field_name(1), id_mpi_pol)
          call calypso_mpi_read_file_open                               &
     &       (rayleigh_field_name(2), id_mpi_tor)
          do j = 1, pick_ra_param%num_modes
            if(my_rank .ne. mod(j,nprocs)) cycle
!
            call read_each_mode_from_rayleigh(id_mpi_pol, ra_rst,       &
     &          pick_ra_param%l_pick(j), pick_ra_param%m_pick(j),       &
     &          ra_picked%poloidal_re, ra_picked%poloidal_im)
            call read_each_mode_from_rayleigh(id_mpi_tor, ra_rst,       &
     &          pick_ra_param%l_pick(j), pick_ra_param%m_pick(j),       &
     &          ra_picked%toroidal_re, ra_picked%toroidal_im)
!
            call set_picked_rayleigh_file_name                          &
     &         (pick_ra_param%picked_prefix, fld_IO%fld_name(i),        &
     &          pick_ra_param%l_pick(j), pick_ra_param%m_pick(j),       &
     &          pick_ra_param%i_step, ra_picked)
            if(     fld_IO%fld_name(i) .eq. velocity%name               &
     &         .or. fld_IO%fld_name(i) .eq. magnetic_field%name) then
              call write_picked_rayleigh_vector(ra_picked)
            else if(fld_IO%fld_name(i) .eq. previous_momentum%name      &
     &         .or. fld_IO%fld_name(i) .eq. previous_induction%name     &
     &              ) then
              call write_picked_rayleigh_prev_vec(ra_picked)
            end if
          end do
          call calypso_close_mpi_file(id_mpi_pol)
          call calypso_close_mpi_file(id_mpi_tor)
        end if
      end do
!
      call dealloc_each_rayleigh_spectr(ra_picked)
!
      end subroutine const_picked_rayleigh_prev_vec
!
! -----------------------------------------------------------------------
!
      end program pickup_rayleigh_spectr

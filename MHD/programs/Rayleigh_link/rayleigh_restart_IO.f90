!>@file   rayleigh_restart_IO.f90
!!@brief  module rayleigh_restart_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine init_rayleigh_restart_input                          &
!!     &         (i_version, dir, i_step, fld_IO)
!!        type(field_IO), intent(inout) :: fld_IO
!!      subroutine find_rayleigh_restart_address                        &
!!     &          (nri, ltr, kr, l, m, ioffset1, ioffset2)
!!@endverbatim
!
      module rayleigh_restart_IO
!
      use m_precision
      use m_constants
      use t_field_data_IO
      use t_rayleigh_restart_IO
      use set_parallel_file_name
!
      implicit  none
!
      private :: set_rayleigh_restart_field
      private :: check_raylegh_rst_field
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_rayleigh_restart_input                            &
     &         (i_version, dir, i_step, fld_IO)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
!
      type(field_IO), intent(inout) :: fld_IO
!
      integer :: ilength
!
!
      if(my_rank .eq. 0) then
        call set_rayleigh_restart_field                                 &
     &     (i_version, dir, i_step, fld_IO)
      end if
!
      call MPI_Bcast(fld_IO%num_field_IO, 1,                            &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(my_rank .ne. 0) call alloc_phys_name_IO(fld_IO)
!
      ilength = int(fld_IO%num_field_IO * kchara)
      call MPI_Bcast(fld_IO%fld_name, ilength,                          &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(fld_IO%num_comp_IO, fld_IO%num_field_IO,           &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      end subroutine init_rayleigh_restart_input
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_restart_field                             &
     &         (i_version, dir, i_step, fld_IO)
!
      use t_base_field_labels
      use t_explicit_term_labels
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
!
      type(field_IO), intent(inout) :: fld_IO
!
!
      fld_IO%num_field_IO = 0
      call alloc_phys_name_IO(fld_IO)
!
      if(     check_raylegh_rst_field(i_version, dir, i_step, wchar)    &
     &  .and. check_raylegh_rst_field(i_version, dir, i_step, zchar))   &
     &   call append_phys_name_IO(velocity%name, n_solenoid, fld_IO)
      if(     check_raylegh_rst_field(i_version, dir, i_step, cchar)    &
     &  .and. check_raylegh_rst_field(i_version, dir, i_step, achar))   &
     &   call append_phys_name_IO(magnetic_field%name, n_solenoid,      &
     &                            fld_IO)
      if(check_raylegh_rst_field(i_version, dir, i_step, pchar))        &
     &   call append_phys_name_IO(pressure%name, n_scalar, fld_IO)
      if(check_raylegh_rst_field(i_version, dir, i_step, tchar))        &
     &   call append_phys_name_IO(temperature%name, n_scalar, fld_IO)
!
!
      if(     check_raylegh_rst_field(i_version, dir, i_step, wabchar)  &
     &  .and. check_raylegh_rst_field(i_version, dir, i_step, zabchar)) &
     &    call append_phys_name_IO(previous_momentum%name, n_solenoid,  &
     &                             fld_IO)
      if(     check_raylegh_rst_field(i_version, dir, i_step, cabchar)  &
     &  .and. check_raylegh_rst_field(i_version, dir, i_step, aabchar)) &
     &    call append_phys_name_IO(previous_induction%name, n_solenoid, &
     &                             fld_IO)
!      if(check_raylegh_rst_field(i_version, dir, i_step, pabchar))     &
!     &   call append_phys_name_IO(previous_pressure%name, n_scalar,    &
!     &                            fld_IO)
      if(check_raylegh_rst_field(i_version, dir, i_step, tabchar))      &
     &   call append_phys_name_IO(previous_heat%name, n_scalar, fld_IO)
!
      end subroutine set_rayleigh_restart_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      logical function check_raylegh_rst_field                          &
     &                (i_version, dir, i_step, sclchar)
!
      use delete_data_files
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: sclchar
!
      character(len = kchara) :: file1
!
!
      file1 = sel_rayleigh_file_name(i_version, dir, i_step, sclchar)
      check_raylegh_rst_field = check_file_exist(file1)
!
      end function check_raylegh_rst_field
!
!-----------------------------------------------------------------------
!
      end module rayleigh_restart_IO

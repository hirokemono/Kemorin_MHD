!>@file   t_rayleigh_restart_IO.f90
!!@brief  module t_rayleigh_restart_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine alloc_rayleigh_radial_grid(ra_rst)
!!      subroutine dealloc_rayleigh_radial_grid(ra_rst)
!!      subroutine bcast_rayleigh_rst_params(ra_rst)
!!      subroutine read_rayleigh_restart_params                         &
!!     &         (dir, i_step, ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      subroutine check_rayleigh_rst_params(id_file, ra_rst)
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!
!!      subroutine set_rayleigh_rst_file_name(dir, i_step, field_name,  &
!!     &          iflag_ncomp, file_name)
!!@endverbatim
!
      module t_rayleigh_restart_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use t_field_data_IO
      use set_parallel_file_name
!
      implicit  none
!
      character(len = kchara), parameter :: paramchar = "grid_etc"
!
      character(len = kchara), parameter :: wchar = "W"
      character(len = kchara), parameter :: zchar = "Z"
      character(len = kchara), parameter :: pchar = "P"
      character(len = kchara), parameter :: tchar = "T"
!
      character(len = kchara), parameter :: cchar = "C"
      character(len = kchara), parameter :: achar = "A"
!
      character(len = kchara), parameter :: wabchar = "WAB"
      character(len = kchara), parameter :: zabchar = "ZAB"
      character(len = kchara), parameter :: pabchar = "PAB"
      character(len = kchara), parameter :: tabchar = "TAB"
!
      character(len = kchara), parameter :: cabchar = "CAB"
      character(len = kchara), parameter :: aabchar = "AAB"
!
!>      Structure for Rayleigh restart data
      type rayleigh_restart
!>        Endian swap flag
        integer(kind = kint) :: iflag_swap = 0
!
!>        truncation degree
        integer(kind = kint) :: ltr_org
!
!>        Radial grid type
        integer(kind = kint) :: iflag_rtype
!>        Number of radial grid
        integer(kind = kint) :: nri_org
!>        radial
        real(kind = kreal), allocatable :: r_org(:)
!
!>        forward transform matrix
        real(kind = kreal), allocatable :: Cheby_fwd(:,:)
!
!>        Original delta t
        real(kind = kreal) :: dt_org
!>        Original time
        real(kind = kreal) :: time_org
!>        new delta t
        real(kind = kreal) :: dt_new
!>        new original delta t
        real(kind = kreal) :: new_dt_org
      end type rayleigh_restart
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_radial_grid(ra_rst)
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      allocate(ra_rst%r_org(ra_rst%nri_org))
      if(ra_rst%nri_org .gt. 0) ra_rst%r_org = 0.0d0
!
      end subroutine alloc_rayleigh_radial_grid
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_radial_grid(ra_rst)
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      deallocate(ra_rst%r_org)
!
      end subroutine dealloc_rayleigh_radial_grid
!
!-----------------------------------------------------------------------
!
      subroutine bcast_rayleigh_rst_params(ra_rst)
!
      use calypso_mpi
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      call MPI_Bcast(ra_rst%iflag_swap, ione,                           &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%ltr_org, ione,                              &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%iflag_rtype, ione,                          &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%nri_org, ione,                              &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_rayleigh_radial_grid(ra_rst)
      call MPI_Bcast(ra_rst%r_org, ra_rst%nri_org,                      &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      call MPI_Bcast(ra_rst%time_org, ione,                             &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%dt_org, ione,                               &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%dt_new, ione,                               &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(ra_rst%new_dt_org, ione,                           &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_rayleigh_rst_params
!
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_restart_params                           &
     &         (dir, i_step, ra_rst)
!
      use binary_IO
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      character(len = kchara) :: file_name
      integer(kind = kint) :: ierr_IO
      integer(kind = kint) :: int_tmp, ilength
!
!
      write(*,*) 'i_step', i_step
      file_name =  set_rayleigh_file_name(dir, i_step, paramchar)
      file_name =  add_null_character(file_name)
      write(*,*) 'read Rayleigh checkpoint paramter file: ',            &
     &          trim(file_name)
      call open_rd_rawfile(file_name, ierr_IO)
!
      ra_rst%iflag_swap = iendian_KEEP
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      if(int_tmp .ne. 4) ra_rst%iflag_swap = iendian_FLIP
!
      call rawread_32bit_f                                              &
     &   (ra_rst%iflag_swap, kint, ra_rst%nri_org, ierr_IO)
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_32bit_f(ra_rst%iflag_swap, kint,                     &
     &               ra_rst%iflag_rtype, ierr_IO)
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_32bit_f                                              &
     &   (ra_rst%iflag_swap, kint, ra_rst%ltr_org, ierr_IO)
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_64bit_f                                              &
     &   (ra_rst%iflag_swap, kreal, ra_rst%dt_org, ierr_IO)
      write(*,*) 'ra_rst%dt_org', ra_rst%dt_org
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_64bit_f                                              &
     &   (ra_rst%iflag_swap, kreal, ra_rst%dt_new, ierr_IO)
      write(*,*) 'ra_rst%dt_new', ra_rst%dt_new
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
!      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!      call rawread_64bit_f(ra_rst%iflag_swap, kreal,                   &
!     &    ra_rst%new_dt_org, ierr_IO)
!      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call alloc_rayleigh_radial_grid(ra_rst)
!
      ilength =  ra_rst%nri_org * kreal
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_64bit_f(ra_rst%iflag_swap, ilength,                  &
     &    ra_rst%r_org(1), ierr_IO)
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
      call rawread_64bit_f(ra_rst%iflag_swap, kreal,                    &
     &   ra_rst%time_org, ierr_IO)
      write(*,*) 'ra_rst%time_org', ra_rst%time_org
      call rawread_32bit_f(ra_rst%iflag_swap, kint, int_tmp, ierr_IO)
!
      call close_binary_file
!
      end subroutine read_rayleigh_restart_params
!
!-----------------------------------------------------------------------
!
      subroutine check_rayleigh_rst_params(id_file, ra_rst)
!
      integer(kind = kint), intent(in) :: id_file
      type(rayleigh_restart), intent(in) :: ra_rst
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) 'iflag_swap', ra_rst%iflag_swap
      write(id_file,*) 'ltr_org',  ra_rst%ltr_org
!
      write(id_file,*) 'iflag_rtype',  ra_rst%iflag_rtype
      write(id_file,*) 'nri_org',  ra_rst%nri_org
      do i = 1,  ra_rst%nri_org
        write(id_file,*) i,  ra_rst%r_org(i)
      end do
!
      write(id_file,*) 'time_org', ra_rst%time_org
      write(id_file,*) 'dt_org',  ra_rst%dt_org
      write(id_file,*) 'dt_new',  ra_rst%dt_new,  ra_rst%new_dt_org
!
      end subroutine check_rayleigh_rst_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_rst_file_name(dir, i_step, field_name,    &
     &          iflag_ncomp, file_name)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: field_name
!
      integer(kind = kint), intent(inout) :: iflag_ncomp
      character(len = kchara), intent(inout) :: file_name(2)
!
      if(field_name .eq. fhd_velo) then
        iflag_ncomp = 2
        file_name(1) =  set_rayleigh_file_name(dir, i_step, wchar)
        file_name(2) =  set_rayleigh_file_name(dir, i_step, zchar)
      else if(field_name .eq. fhd_press) then
        iflag_ncomp = 1
        file_name(1) =  set_rayleigh_file_name(dir, i_step, pchar)
      else if(field_name .eq. fhd_temp) then
        iflag_ncomp = 1
        file_name(1) =  set_rayleigh_file_name(dir, i_step, tchar)
      else if(field_name .eq. fhd_magne) then
        iflag_ncomp = 2
        file_name(1) =  set_rayleigh_file_name(dir, i_step, cchar)
        file_name(2) =  set_rayleigh_file_name(dir, i_step, achar)
!
      else if(field_name .eq. fhd_pre_mom) then
        iflag_ncomp = 2
        file_name(1) =  set_rayleigh_file_name(dir, i_step, wabchar)
        file_name(2) =  set_rayleigh_file_name(dir, i_step, zabchar)
!      else if(field_name .eq. fhd_pre_press) then
!        iflag_ncomp = 1
!        file_name(1) =  set_rayleigh_file_name(dir, i_step, cchar)
      else if(field_name .eq. fhd_pre_heat) then
        iflag_ncomp = 1
        file_name(1) =  set_rayleigh_file_name(dir, i_step, tabchar)
      else if(field_name .eq. fhd_pre_uxb) then
        iflag_ncomp = 2
        file_name(1) =  set_rayleigh_file_name(dir, i_step, cabchar)
        file_name(2) =  set_rayleigh_file_name(dir, i_step, aabchar)
      end if
!
      end subroutine set_rayleigh_rst_file_name
!
!-----------------------------------------------------------------------
!
      end module t_rayleigh_restart_IO

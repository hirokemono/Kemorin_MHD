!>@file   MPI_read_rayleigh_restart.f90
!!@brief  module MPI_read_rayleigh_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2018
!
!>@brief  MPI routines for Rayleigh restart IO
!!
!!@verbatim
!!      subroutine bcast_rayleigh_restart_param(ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!      subroutine find_rayleigh_restart_address                        &
!!     &         (nri, ltr, kr, l, m, ioffset1, ioffset2)
!!      subroutine read_each_mode_from_rayleigh                         &
!!     &         (id_mpi_file, ra_rst, l, m, field_re, field_im)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = kint), intent(in) :: l, m
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        real(kind = kreal), intent(inout) :: field_re(ra_rst%nri_org)
!!        real(kind = kreal), intent(inout) :: field_im(ra_rst%nri_org)
!!
!!      subroutine simple_read_rayleigh_spectr                          &
!!     &         (file_name, ra_rst, read_fld)
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        character(len = kchara), intent(in) :: file_name
!!        real(kind = kreal), intent(inout)                             &
!!     &  :: read_fld(ra_rst%nri_org,0:ra_rst%ltr_org*(ra_rst%ltr_org+2))
!!@endverbatim
!
      module MPI_read_rayleigh_restart
!
      use m_precision
      use calypso_mpi
!
      use t_rayleigh_restart_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine bcast_rayleigh_restart_param(ra_rst)
!
      use calypso_mpi_int4
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
!
      call calypso_mpi_bcast_one_int4(ra_rst%i_version_from_file, 0)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%iflag_swap' 
      call calypso_mpi_bcast_one_int4(ra_rst%iflag_swap, 0)
!
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%ltr_org' 
      call MPI_Bcast(ra_rst%ltr_org, 1,                                 &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%iflag_rtype' 
      call MPI_Bcast(ra_rst%iflag_rtype, 1,                             &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%nri_org' 
      call MPI_Bcast(ra_rst%nri_org, 1,                                 &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_rayleigh_radial_grid(ra_rst)
      call calypso_mpi_barrier
!
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%nri_org' 
      call MPI_Bcast(ra_rst%r_org, ra_rst%nri_org,                      &
     &    CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%time_org' 
      call MPI_Bcast(ra_rst%time_org, 1,                                &
     &    CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%dt_org' 
      call MPI_Bcast(ra_rst%dt_org, 1,                                  &
     &    CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%dt_new' 
      call MPI_Bcast(ra_rst%dt_new, 1,                                  &
     &    CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%new_dt_org' 
      call MPI_Bcast(ra_rst%new_dt_org, 1,                              &
     &    CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%i_step_org' 
      call MPI_Bcast(ra_rst%i_step_org, 1,                              &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_rayleigh_restart_param
!
! -----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_rayleigh_restart_address                          &
     &         (nri, ltr, kr, l, m, ioffset1, ioffset2)
!
      integer(kind = kint), intent(in) :: nri, ltr, kr, l, m
      integer(kind = kint_gl), intent(inout) :: ioffset1, ioffset2
!
      integer(kind = kint_gl) :: jmax_h, ioffset
!
      jmax_h = 1 + ltr*(ltr+3) / 2
      ioffset = (l - m + 1) + m * (2*ltr +3 - m) / 2
      ioffset1 = ioffset + (kr - 1) * jmax_h
      ioffset2 = ioffset1 + jmax_h * nri
      ioffset1 = (ioffset1 - 1) * kreal
      ioffset2 = (ioffset2 - 1) * kreal
!
      end subroutine find_rayleigh_restart_address
!
!-----------------------------------------------------------------------
!
      subroutine read_each_mode_from_rayleigh                           &
     &         (id_mpi_file, ra_rst, l, m, field_re, field_im)
!
      use m_calypso_mpi_IO
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: l, m
      type(rayleigh_restart), intent(in) :: ra_rst
!
      real(kind = kreal), intent(inout) :: field_re(ra_rst%nri_org)
      real(kind = kreal), intent(inout) :: field_im(ra_rst%nri_org)
!
      integer(kind = kint) :: k
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      do k = 1, ra_rst%nri_org
        call find_rayleigh_restart_address                              &
     &     (ra_rst%nri_org, ra_rst%ltr_org,                             &
     &      k, l, abs(m), ioffset1, ioffset2)
!
        call mpi_read_real_b(id_mpi_file, ra_rst%iflag_swap,            &
     &                       ioffset1, ione64, field_re(k))
        call mpi_read_real_b(id_mpi_file, ra_rst%iflag_swap,            &
     &                       ioffset2, ione64, field_im(k))
      end do
!
      end subroutine read_each_mode_from_rayleigh
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine simple_read_rayleigh_spectr                            &
     &         (file_name, ra_rst, read_fld)
!
      use m_calypso_mpi_IO
!
      type(rayleigh_restart), intent(in) :: ra_rst
      character(len = kchara), intent(in) :: file_name
!
      real(kind = kreal), intent(inout)                                 &
     &  :: read_fld(ra_rst%nri_org,0:ra_rst%ltr_org*(ra_rst%ltr_org+2))
!
      integer ::  id_mpi_file
!
      integer(kind = kint) :: l, m, j1, j2
      integer(kind = kint_gl), parameter :: ione64 = 1
      real(kind = kreal) :: field_im(ra_rst%nri_org)
!
!
!
      call calypso_mpi_read_file_open(file_name, id_mpi_file)
      if(my_rank .eq. 0) then
        do l = 0, ra_rst%ltr_org
          do m = 0, l
            j1 = l*(l+1) + m
            j2 = l*(l+1) - m
!
            call read_each_mode_from_rayleigh(id_mpi_file, ra_rst,      &
     &          l, m, read_fld(1,j1), field_im(1))
!
            if(m .gt. 0) read_fld(1:ra_rst%nri_org,j2)                  &
     &                       = field_im(1:ra_rst%nri_org)
          end do
        end do
!
      end if
      call calypso_close_mpi_file(id_mpi_file)
!
      end subroutine simple_read_rayleigh_spectr
!
! -----------------------------------------------------------------------
!
      end module MPI_read_rayleigh_restart

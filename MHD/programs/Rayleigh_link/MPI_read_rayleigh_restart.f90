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
!!     &         (id_mpi_file, l, m, ra_rst, rayleigh_in)
!!        integer, intent(in) ::  id_mpi_file
!!        integer(kind = kint), intent(in) :: l, m
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: rayleigh_in(ra_rst%nri_org,2)
!!
!!      subroutine check_rayleigh_restart_reading                       &
!!     &         (file_name, i_comp, ra_rst)
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        character(len = kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: i_comp
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
      type(rayleigh_restart), intent(inout) :: ra_rst
!
!
      call MPI_Bcast(ra_rst%i_version_from_file, 1,                     &
     &    CALYPSO_FOUR_INT, 0, CALYPSO_COMM, ierr_MPI)
!      if(my_rank .eq. 0) write(*,*) 'MPI_Bcast ra_rst%iflag_swap' 
      call MPI_Bcast(ra_rst%iflag_swap, 1,                              &
     &    CALYPSO_FOUR_INT, 0, CALYPSO_COMM, ierr_MPI)
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
     &         (id_mpi_file, l, m, ra_rst, rayleigh_in)
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      integer, intent(in) ::  id_mpi_file
      integer(kind = kint), intent(in) :: l, m
      type(rayleigh_restart), intent(in) :: ra_rst
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rayleigh_in(ra_rst%nri_org,2)
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
        call calypso_mpi_seek_read_real                                 &
     &     (id_mpi_file, ra_rst%iflag_swap,                             &
     &      ioffset1, ione64, rayleigh_in(k,1))
        call calypso_mpi_seek_read_real                                 &
     &     (id_mpi_file, ra_rst%iflag_swap,                             &
     &      ioffset2, ione64, rayleigh_in(k,2))
      end do
!
      end subroutine read_each_mode_from_rayleigh
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_rayleigh_restart_reading                         &
     &         (file_name, i_comp, ra_rst)
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      type(rayleigh_restart), intent(in) :: ra_rst
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_comp
!
      integer ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
!
      integer(kind = kint_gl) :: k, j, inod
      character(len = kchara) :: fn_out
      integer(kind = kint_gl) :: jmax_h
      real(kind = kreal) :: read_fld(2)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call calypso_mpi_read_file_open(file_name, id_mpi_file)
      if(my_rank .eq. 0) then
        write(fn_out,'(a,i1)') 'rayleigh_test.', i_comp
        open(99,file=fn_out)
!
        jmax_h = 1 + ra_rst%ltr_org*(ra_rst%ltr_org+3) / 2
        do j = 1, jmax_h
          do k = 1, ra_rst%nri_org
            inod = k + (j-1) * ra_rst%nri_org
            ioffset1 = (inod-1) * kreal
            ioffset2 = ioffset1 + kreal*ra_rst%nri_org*jmax_h
            call calypso_mpi_seek_read_real                             &
     &         (id_mpi_file, ra_rst%iflag_swap,                         &
     &          ioffset1, ione64, read_fld(1))
            call calypso_mpi_seek_read_real                             &
     &         (id_mpi_file, ra_rst%iflag_swap,                         &
     &          ioffset2, ione64, read_fld(2))
!
            write(99,*) inod, read_fld(1:2)
          end do
        end do
!
        close(99)
      end if
      call calypso_close_mpi_file(id_mpi_file)
!
      end subroutine check_rayleigh_restart_reading
!
! -----------------------------------------------------------------------
!
      end module MPI_read_rayleigh_restart

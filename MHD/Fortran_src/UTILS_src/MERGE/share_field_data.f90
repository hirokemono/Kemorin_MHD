!>@file   share_field_data.f90
!!@brief  module share_field_data
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2011
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine share_phys_field_names(fld)
!!        type(phys_data), intent(inout) :: fld
!!      subroutine share_time_step_data(time_d)
!!        type(time_data), intent(inout) :: time_d
!!      subroutine share_each_field_data(ip_org, fld)
!!        type(phys_data), intent(inout) :: fld
!!@endverbatim
!
      module share_field_data
!
      use m_precision
!
      use calypso_mpi
      use t_time_data
      use t_phys_data
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_phys_field_names(fld)
!
      type(phys_data), intent(inout) :: fld
!
!        write(*,*) 'MPI_Bcast num_phys', ip
      call MPI_Bcast(fld%num_phys, ione,                                &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast ntot_phys', ip
      call MPI_Bcast(fld%ntot_phys, ione,                               &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. 0) call alloc_phys_name_type(fld)
!
!      write(*,*) 'MPI_Bcast num_component', ip
      call MPI_Bcast(fld%num_component, fld%num_phys,                   &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast istack_component', ip
      call MPI_Bcast(fld%istack_component, (fld%num_phys+1),            &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast iflag_monitor', ip
      call MPI_Bcast(fld%iflag_monitor, fld%num_phys,                   &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!        write(*,*) 'MPI_Bcast phys_name', ip
      call MPI_Bcast(fld%phys_name, (fld%num_phys*kchara),              &
     &    CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_phys_field_names
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_time_step_data(time_d)
!
      use t_time_data
!
      type(time_data), intent(inout) :: time_d
!
!
      call MPI_Bcast(time_d%i_time_step, ione, CALYPSO_INTEGER,         &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%time, ione, CALYPSO_REAL,                   &
     &    izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(time_d%dt, ione, CALYPSO_REAL,                     &
     &    izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_time_step_data
!
! -----------------------------------------------------------------------
!
      subroutine share_each_field_data(ip_org, fld)
!
      integer(kind = kint), intent(in) :: ip_org
      type(phys_data), intent(inout) :: fld
!
      integer(kind = kint) ::  irank_org, num
!
!
      irank_org = mod(ip_org-1,nprocs)
!        write(*,*) 'MPI_Bcast num_neib', ip_org
      call MPI_Bcast(fld%n_point, ione, CALYPSO_INTEGER,                &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(irank_org,nprocs) .ne. my_rank) then
        num = fld%n_point
        call alloc_phys_data_type(num, fld)
      end if
!
      num = fld%ntot_phys * fld%n_point
      call MPI_Bcast(fld%d_fld, num, CALYPSO_REAL,                      &
     &    irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_each_field_data
!
! -----------------------------------------------------------------------
!
      end module share_field_data

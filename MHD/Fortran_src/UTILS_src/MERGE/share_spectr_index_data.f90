!>@file   share_spectr_index_data.f90
!!@brief  module share_spectr_index_data
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief routines for parallel spectr data assemble
!!
!!@verbatim
!!      subroutine share_sph_rj_data(ip_org, sph_mesh)
!!        type(sph_mesh_data), intent(inout) :: sph_mesh
!!@endverbatim
!!
!
      module share_spectr_index_data
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_SPH_mesh_field_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine share_sph_rj_data(ip_org, sph_mesh)
!
      integer(kind = kint), intent(in) :: ip_org
      type(sph_mesh_data), intent(inout) :: sph_mesh
!
      integer :: irank_org
!
!
      irank_org = int(mod(ip_org - 1,nprocs))
!      write(*,*) 'MPI_Bcast irank_sph_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%irank_sph_rj,                  &
     &    itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast nidx_global_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%nidx_global_rj,                &
     &    itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast nnod_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%nnod_rj,                       &
     &    ione, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast nidx_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%nidx_rj,                       &
     &    itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast ist_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%ist_rj,                        &
     &    itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast ied_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%ied_rj,                        &
     &    itwo, CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      if(mod(ip_org-1,nprocs) .ne. my_rank) then
        call alloc_type_spheric_param_rj(sph_mesh%sph%sph_rj)
        call alloc_type_sph_1d_index_rj(sph_mesh%sph%sph_rj)
      end if
!
!      write(*,*) 'MPI_Bcast idx_global_rj', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%idx_global_rj,                 &
     &    (2*sph_mesh%sph%sph_rj%nnod_rj),                              &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
!      write(*,*) 'MPI_Bcast radius_1d_rj_r', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%radius_1d_rj_r,                &
     &    sph_mesh%sph%sph_rj%nidx_rj(1),                               &
     &    CALYPSO_REAL, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast idx_gl_1d_rj_r', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%idx_gl_1d_rj_r,                &
     &    sph_mesh%sph%sph_rj%nidx_rj(1),                               &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!      write(*,*) 'MPI_Bcast idx_gl_1d_rj_j', ip_org
      call MPI_Bcast(sph_mesh%sph%sph_rj%idx_gl_1d_rj_j,                &
     &    (3*sph_mesh%sph%sph_rj%nidx_rj(2)),                           &
     &    CALYPSO_INTEGER, irank_org, CALYPSO_COMM, ierr_MPI)
!
      end subroutine share_sph_rj_data
!
! -----------------------------------------------------------------------
!
      end module share_spectr_index_data

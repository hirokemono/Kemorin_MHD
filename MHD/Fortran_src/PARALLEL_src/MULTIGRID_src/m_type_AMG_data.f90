!m_type_AMG_data.f90
!      module m_type_AMG_data
!
!     Written by H. Matsui on Dec., 2008
!
      module m_type_AMG_data
!
      use m_precision
!
      use t_comm_table
      use t_vector_for_solver
      use t_solver_djds
!
      implicit  none
!
!
!   Maximum Levels of multigrid (level 0 is original mesh)
      integer(kind = kint), parameter :: max_MG_level = 12
!   Levels of multigrid (level 0 is original mesh)
      integer(kind = kint) :: num_MG_level = 1
!
!
!>     structure of communicator for MGCG
      type(mpi_4_solver), target, save :: MG_mpi(0:max_MG_level)
!>     structure of vectors in MGCG
      type(vectors_4_solver), target, save :: MG_vector(0:max_MG_level)
!
      type(DJDS_MATRIX) :: MG_mat(0:max_MG_level)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine split_multigrid_comms
!
      use calypso_mpi
!
      integer(kind = kint) :: ilevel, my_rank4, nprocs_mg4, my_rank_mg4
!
!
      MG_mpi(0)%icolor_MG = 0
!
      call MPI_COMM_DUP(CALYPSO_COMM, MG_mpi(0)%SOLVER_COMM, ierr_MPI)
      call MPI_COMM_RANK(MG_mpi(0)%SOLVER_COMM, my_rank_mg4, ierr_MPI)
      MG_mpi(0)%MG_rank = my_rank_mg4
      MG_mpi(0)%nprocs =  nprocs
!
      do ilevel = 1, num_MG_level
        if(my_rank .lt. MG_mpi(ilevel)%nprocs) then
          MG_mpi(ilevel)%icolor_MG = 0
        else
          MG_mpi(ilevel)%icolor_MG = 1
        end if
!
        my_rank4 = int(my_rank)
        call MPI_COMM_SPLIT(CALYPSO_COMM, MG_mpi(ilevel)%icolor_MG,     &
     &      my_rank4, MG_mpi(ilevel)%SOLVER_COMM, ierr_MPI)
        call MPI_COMM_SIZE                                              &
     &     (MG_mpi(ilevel)%SOLVER_COMM, nprocs_mg4, ierr_MPI)
        call MPI_COMM_RANK                                              &
     &     (MG_mpi(ilevel)%SOLVER_COMM, my_rank_mg4, ierr_MPI)
        MG_mpi(ilevel)%MG_rank = my_rank_mg4
      end do
!
      end subroutine split_multigrid_comms
!
!  ---------------------------------------------------------------------
!
      end module m_type_AMG_data

!t_MGCG_data.f90
!      module t_MGCG_data
!
!     Written by H. Matsui on Dec., 2008
!
!!@verbatim
!!      subroutine alloc_MGCG_data(num_level, MGCG_WK)
!!      subroutine alloc_MGCG_mesh(MGCG_WK, MGCG_FEM)
!!
!!      subroutine dealloc_MGCG_data(MGCG_WK)
!!      subroutine dealloc_MGCG_mesh(MGCG_FEM)
!!
!!      subroutine split_multigrid_comms(MGCG_WK)
!!      subroutine set_ctl_data_4_Multigrid                             &
!!     &         (MG_ctl, MG_param, MG_file, MGCG_WK, MGCG_FEM)
!!       type(MGCG_control), intent(inout) :: MG_ctl
!!       type(MGCG_parameter), intent(inout) :: MG_param
!!       type(MGCG_file_list), intent(inout) :: MG_file
!!       type(MGCG_data), intent(inout) :: MGCG_WK
!!       type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!!@endverbatim
!
      module t_MGCG_data
!
      use m_precision
!
      use t_comm_table
      use t_vector_for_solver
      use t_solver_djds
!
      use t_mesh_data
      use t_interpolate_table
      use t_jacobians
      use t_table_FEM_const
      use t_work_FEM_integration
!
      implicit  none
!
!
!>      Structure for MGCG solver
      type MGCG_data
!         Levels of multigrid (level 0 is original mesh)
        integer(kind = kint) :: num_MG_level = 1
!
!>        structure of communicator for MGCG
        type(mpi_4_solver), allocatable :: MG_mpi(:)
!>        structure of vectors in MGCG
        type(vectors_4_solver), allocatable :: MG_vector(:)
!
!>        structure of communicator for MGCG matrix
        type(DJDS_MATRIX), allocatable :: MG_mat(:)
      end type MGCG_data
!
!
      type mesh_4_MGCG
!>        Mesh structure for Multigrid
        type(mesh_data), allocatable :: MG_mesh(:)
!>        Element structure for Multigrid
        type(element_geometry), allocatable ::  MG_ele_mesh(:)
!
!>        Element flag for Multigrid
        integer(kind = kint) :: iflag_MG_commute_by_ele = 0
!>        Element interpolation table
        type(interpolate_table), allocatable :: MG_c2f_ele_tbl(:)
!
!>        Stracture for FEM assembling
        type(finite_element_integration), allocatable :: MG_FEM_int(:)
!>        FEM assemble table for multigrid mesh
        type(arrays_finite_element_mat), allocatable :: MG_FEM_mat(:)
      end type mesh_4_MGCG
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_MGCG_data(num_level, MGCG_WK)
!
      integer(kind = kint), intent(in) :: num_level
      type(MGCG_data), intent(inout) :: MGCG_WK
!
!
      MGCG_WK%num_MG_level = num_level
!
      allocate(MGCG_WK%MG_mpi(0:MGCG_WK%num_MG_level))
      allocate(MGCG_WK%MG_vector(0:MGCG_WK%num_MG_level))
!
      end subroutine alloc_MGCG_data
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_MGCG_mesh(MGCG_WK, MGCG_FEM)
!
      type(MGCG_data), intent(in) :: MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!
!
      allocate(MGCG_FEM%MG_mesh(MGCG_WK%num_MG_level))
      allocate(MGCG_FEM%MG_ele_mesh(MGCG_WK%num_MG_level))
!
      allocate(MGCG_FEM%MG_c2f_ele_tbl(MGCG_WK%num_MG_level))
!
      allocate(MGCG_FEM%MG_FEM_mat(MGCG_WK%num_MG_level))
      allocate(MGCG_FEM%MG_FEM_int(MGCG_WK%num_MG_level))
!
      end subroutine alloc_MGCG_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_MGCG_data(MGCG_WK)
!
      type(MGCG_data), intent(inout) :: MGCG_WK
!
!
      deallocate(MGCG_WK%MG_mpi)
      deallocate(MGCG_WK%MG_vector)
!
      end subroutine dealloc_MGCG_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_MGCG_mesh(MGCG_FEM)
!
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!
!
      deallocate(MGCG_FEM%MG_mesh)
      deallocate(MGCG_FEM%MG_ele_mesh)
!
      deallocate(MGCG_FEM%MG_c2f_ele_tbl)
!
      deallocate(MGCG_FEM%MG_FEM_int)
      deallocate(MGCG_FEM%MG_FEM_mat)
!
      end subroutine dealloc_MGCG_mesh
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine split_multigrid_comms(MGCG_WK)
!
      use calypso_mpi
!
      type(MGCG_data), intent(inout) :: MGCG_WK
!
      integer(kind = kint) :: ilevel, my_rank4, nprocs_mg4, my_rank_mg4
!
!
       MGCG_WK%MG_mpi(0)%icolor_MG = 0
!
      call MPI_COMM_DUP                                                 &
     &   (CALYPSO_COMM, MGCG_WK%MG_mpi(0)%SOLVER_COMM, ierr_MPI)
      call MPI_COMM_RANK                                                &
     &   (MGCG_WK%MG_mpi(0)%SOLVER_COMM, my_rank_mg4, ierr_MPI)
       MGCG_WK%MG_mpi(0)%MG_rank = my_rank_mg4
       MGCG_WK%MG_mpi(0)%nprocs =  nprocs
!
      do ilevel = 1, MGCG_WK%num_MG_level
        if(my_rank .lt. MGCG_WK%MG_mpi(ilevel)%nprocs) then
          MGCG_WK%MG_mpi(ilevel)%icolor_MG = 0
        else
          MGCG_WK%MG_mpi(ilevel)%icolor_MG = 1
        end if
!
        my_rank4 = int(my_rank)
        call MPI_COMM_SPLIT                                             &
     &     (CALYPSO_COMM, MGCG_WK%MG_mpi(ilevel)%icolor_MG,             &
     &      my_rank4, MGCG_WK%MG_mpi(ilevel)%SOLVER_COMM, ierr_MPI)
        call MPI_COMM_SIZE                                              &
     &     (MGCG_WK%MG_mpi(ilevel)%SOLVER_COMM, nprocs_mg4, ierr_MPI)
        call MPI_COMM_RANK                                              &
     &     (MGCG_WK%MG_mpi(ilevel)%SOLVER_COMM, my_rank_mg4, ierr_MPI)
        MGCG_WK%MG_mpi(ilevel)%MG_rank = my_rank_mg4
      end do
!
      end subroutine split_multigrid_comms
!
!------------------------------------------------------------------
!
      subroutine set_ctl_data_4_Multigrid                               &
     &         (MG_ctl, MG_param, MG_file, MGCG_WK, MGCG_FEM)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_file_format_switch
      use t_MGCG_parameter
      use t_ctl_data_4_Multigrid
      use set_parallel_file_name
!
      type(MGCG_control), intent(inout) :: MG_ctl
      type(MGCG_parameter), intent(inout) :: MG_param
      type(MGCG_file_list), intent(inout) :: MG_file
      type(MGCG_data), intent(inout) :: MGCG_WK
      type(mesh_4_MGCG), intent(inout) :: MGCG_FEM
!
      integer(kind = kint) :: i
!
!
      call set_MGCG_parameter(MG_ctl, MG_param)
!
      if (MG_ctl%num_multigrid_level_ctl%iflag .eq. 0) then
        MG_ctl%num_multigrid_level_ctl%intvalue = 0
      end if
!
      call alloc_MGCG_data                                              &
     &   (MG_ctl%num_multigrid_level_ctl%intvalue, MGCG_WK)
      call alloc_MGCG_mesh(MGCG_WK, MGCG_FEM)
!
      if (MGCG_WK%num_MG_level .gt. 0) then
        if(MG_ctl%num_MG_subdomain_ctl%num .ne. MGCG_WK%num_MG_level)   &
     &   then
          write(e_message,'(a)')                                        &
     &            'set correct level for MG subdomains'
          call calypso_MPI_abort(ierr_CG, e_message)
        end if
!
        MGCG_WK%MG_mpi(1:MGCG_WK%num_MG_level)%nprocs                   &
     &     = MG_ctl%num_MG_subdomain_ctl%ivec(1:MGCG_WK%num_MG_level)
        call dealloc_control_array_int(MG_ctl%num_MG_subdomain_ctl)
!
        if (MG_ctl%MG_f2c_ele_tbl_ctl%icou .eq. MG_file%nlevel_f) then
          MGCG_FEM%iflag_MG_commute_by_ele = 1
        end if
      end if
!
      if (iflag_debug .gt. 0) then
        do i = 1, MG_file%nlevel_f
          write(*,*) '# of domains for level ', i, ':  ',              &
     &               MGCG_WK%MG_mpi(i)%nprocs
        end do
      end if
!
      call set_MGCG_file_controls                                      &
     &   (MGCG_WK%num_MG_level, MG_ctl, MG_file)
!
      end subroutine set_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module t_MGCG_data

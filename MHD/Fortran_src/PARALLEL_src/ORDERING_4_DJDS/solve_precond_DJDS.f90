!
!      module solve_precond_DJDS
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine solve_by_djds_solver11                               &
!!     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,&
!!     &          itr_res, ierr)
!!      subroutine solve_by_djds_solver33                               &
!!     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,&
!!     &          itr_res, ierr)
!!      subroutine solve_by_djds_solverNN                               &
!!     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,&
!!     &          itr_res, ierr)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(CG_poarameter), intent(in) :: CG_param
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!        type(DJDS_MATRIX), intent(inout) :: djds_mat
!!
!!     solve by DJDS solver using CRS matrix
!!     results are also copied to CRS array
!
      module solve_precond_DJDS
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_geometry_data
      use t_comm_table
      use t_crs_matrix
!
      implicit none
!
!>    RHS and solution vector
      real(kind=kreal), allocatable :: b_djds(:), x_djds(:)
!
      real(kind = kreal) :: starttime
!
      private :: allocate_vector_data_4_djds
      private :: deallocate_vector_data_4_djds
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_vector_data_4_djds(NP, NB)
!
       integer(kind = kint), intent(in) :: NP, NB
!
       allocate (b_djds(NB*NP))
       allocate (x_djds(NB*NP))
!
       b_djds = 0.0d0
       x_djds = 0.0d0
!
      end subroutine allocate_vector_data_4_djds
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_vector_data_4_djds
!
       deallocate (b_djds, x_djds)
!
      end subroutine deallocate_vector_data_4_djds
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_djds_RHS_vector(my_rank, djds_mat)
!
      use t_solver_djds
!
      integer (kind = kint), intent(in) :: my_rank
      type(DJDS_MATRIX), intent(in) :: djds_mat
!
      integer(kind = kint) :: i, NB_djds, k2
!
!
       NB_djds = djds_mat%NB
       do i = 1, djds_mat%num_diag
           write(my_rank+50,*) "vector (inod) = ", i
           write(my_rank+50,'(1p5e16.8)')                               &
     &           (b_djds(NB_djds*(i-1)+k2),k2=1,NB_djds)
       end do
!
       end subroutine check_djds_RHS_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solver11                                 &
     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,  &
     &          itr_res, ierr)
!
      use m_solver_SR
      use t_iccg_parameter
      use t_solver_djds
!
      use solver_DJDS11_struct
!
      use copy_matrix_2_djds_array
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(CG_poarameter), intent(in) :: CG_param
      type(CRS_matrix), intent(inout) :: mat_crs
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: djds_mat
!
      integer(kind = kint), intent(inout) :: ierr, itr_res
!
!
      call allocate_vector_data_4_djds(node%numnod, djds_mat%NB)
      call copy_RH_vect_2_crs_nn                                        &
     &   (mat_crs, node%numnod, djds_mat%NB, b_djds, x_djds)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER(CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()
 
        ierr = 1
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR                                         &
     &       (ione, nod_comm%num_neib, nod_comm%num_neib,               &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

      call precond_DJDS11_struct(np_smp, djds_tbl, djds_mat,            &
     &    CG_param%PRECOND, CG_param%sigma_diag)
!C
!C-- ICCG computation

        write(*,*) 'init_solve_DJDS_kemo', my_rank
      call init_solve_DJDS11_struct(np_smp, nod_comm,                   &
     &    djds_tbl, djds_mat, node%numnod, b_djds, x_djds,              &
     &    CG_param%METHOD, CG_param%PRECOND, ierr,                      &
     &    CG_param%EPS, CG_param%MAXIT, itr_res)

      call copy_solution_2_crs_nn                                       &
     &   (node%numnod, djds_mat%NB, x_djds, mat_crs)
      call deallocate_vector_data_4_djds
!
      end  subroutine solve_by_djds_solver11
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solver33                                 &
     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,  &
     &          itr_res, ierr)
!
      use m_solver_SR
      use t_iccg_parameter
      use t_solver_djds
!
      use solver_DJDS33_struct

      use copy_matrix_2_djds_array
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(CG_poarameter), intent(in) :: CG_param
      type(CRS_matrix), intent(inout) :: mat_crs
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: djds_mat
!
      integer(kind = kint), intent(inout) :: ierr, itr_res
!
!
      call allocate_vector_data_4_djds(node%numnod, djds_mat%NB)
      call copy_RH_vect_2_crs_nn                                        &
     &   (mat_crs, node%numnod, djds_mat%NB, b_djds, x_djds)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()
!
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR                                         &
     &       (ithree, nod_comm%num_neib, nod_comm%num_neib,             &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

        call precond_DJDS33_struct(np_smp, djds_tbl, djds_mat,          &
     &      CG_param%PRECOND, CG_param%sigma_diag)
!C
!C-- ICCG computation

        ierr = 1
 
        write(*,*) 'init_solve33_DJDS_struct', CG_param%METHOD
      call init_solve33_DJDS_struct(np_smp, nod_comm,                   &
     &    djds_tbl, djds_mat, node%numnod, b_djds, x_djds,              &
     &    CG_param%METHOD, CG_param%PRECOND, ierr,                      &
     &    CG_param%EPS, CG_param%MAXIT, itr_res)

      call copy_solution_2_crs_nn                                       &
     &   (node%numnod, djds_mat%NB, x_djds, mat_crs)
      call deallocate_vector_data_4_djds
!
      end  subroutine solve_by_djds_solver33
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solverNN                                 &
     &         (node, nod_comm, CG_param, mat_crs, djds_tbl, djds_mat,  &
     &          itr_res, ierr)
!
      use m_solver_SR
      use t_iccg_parameter
      use t_solver_djds
!
      use solver_DJDSnn_struct

      use copy_matrix_2_djds_array
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(CG_poarameter), intent(in) :: CG_param
      type(CRS_matrix), intent(inout) :: mat_crs
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: djds_mat
!
      integer(kind = kint), intent(inout) :: itr_res, ierr
!
!
      call allocate_vector_data_4_djds(node%numnod, djds_mat%NB)
      call copy_RH_vect_2_crs_nn                                        &
     &   (mat_crs, node%numnod, djds_mat%NB, b_djds, x_djds)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()

        write(*,*) 'resize_work_4_SR'
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR                                         &
     &       (djds_mat%NB, nod_comm%num_neib, nod_comm%num_neib,        &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

      write(*,*) 'precond_DJDSNN'
      call precond_DJDSnn_struct(djds_mat%NB, np_smp, djds_tbl,         &
     &   djds_mat, CG_param%PRECOND, CG_param%sigma_diag)
!C
!C-- ICCG computation

        ierr = 1
!       if (my_rank.eq.0) write(*,*) 'init_solveNN_DJDS_struct'
      call init_solveNN_DJDS_struct(djds_mat%NB, np_smp, nod_comm,      &
     &    djds_tbl, djds_mat, node%numnod, b_djds, x_djds,              &
     &    CG_param%METHOD, CG_param%PRECOND, ierr,                      &
     &    CG_param%EPS, CG_param%MAXIT, itr_res)

      call copy_solution_2_crs_nn                                       &
     &   (node%numnod, djds_mat%NB, x_djds, mat_crs)
      call deallocate_vector_data_4_djds
!
      end  subroutine solve_by_djds_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_precond_DJDS

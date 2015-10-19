!
!      module solve_precond_DJDS
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!!      subroutine solve_by_djds_solver11(node, tbl_crs, ierr)
!!      subroutine solve_by_djds_solver33(node, tbl_crs, ierr)
!!      subroutine solve_by_djds_solverNN(node, tbl_crs, ierr)
!!        type(node_data), intent(inout) :: node
!!        type(CRS_matrix_connect), intent(inout) :: tbl_crs
!!
!!     solve by DJDS solver using CRS matrix
!!     results are also copied to CRS array
!
      module solve_precond_DJDS
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_crs_matrix
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solver11(node, tbl_crs, ierr)
!
      use calypso_mpi
      use m_nod_comm_table
      use m_machine_parameter
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
!
      use m_solver_SR
      use solver_DJDS
      use preconditioning_DJDS11
!
      use transfer_crs_2_djds
      use copy_matrix_2_djds_array
!
      type(node_data), intent(inout) :: node
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      integer(kind = kint), intent(inout) :: ierr
!
!
      call transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER(CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()
 
        ierr = 1
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR(ione, nod_comm%num_neib,                &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

        call precond_DJDS11(node%internal_node, node%numnod,            &
     &           NLmax, itotal_l, NHYP, np_smp,                         &
     &           node%istack_internal_smp, STACKmc, NLmaxHYP, IVECT,    &
     &           OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU,                &
     &           aiccg(im_d), indexDJDS_L, itemDJDS_L, aiccg(im_l),     &
     &           ALUG_L, ALUG_U, precond_4_solver, sigma_diag)
!
!C
!C-- ICCG computation

        write(*,*) 'init_solve_DJDS_kemo', my_rank
      call init_solve_DJDS_kemo(node%internal_node, node%numnod,        &
     &     NLmax, NUmax, itotal_l, itotal_u, NHYP, np_smp,              &
     &     node%istack_internal_smp, STACKmc, NLmaxHYP, NUmaxHYP,       &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u), ALUG_L, ALUG_U, eps, itr, ierr,    &
     &     nod_comm%num_neib, nod_comm%id_neib,                         &
     &     nod_comm%istack_import, nod_comm%item_import,                &
     &     nod_comm%istack_export, NOD_EXPORT_NEW,                      &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn(node%numnod)

!
      end  subroutine solve_by_djds_solver11
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solver33(node, tbl_crs, ierr)
!
      use calypso_mpi
      use m_nod_comm_table
      use m_machine_parameter
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
!
      use m_solver_SR
      use solver33_DJDS
      use preconditioning_DJDS33

      use transfer_crs_2_djds
      use copy_matrix_2_djds_array
!
      type(node_data), intent(inout) :: node
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      integer(kind = kint), intent(inout) :: ierr
!
!
      call transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()
!
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR(ithree, nod_comm%num_neib,              &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

        call precond_DJDS33(node%internal_node, node%numnod,            &
     &           np_smp, node%istack_internal_smp,                      &
     &           OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                      &
     &           aiccg(im_d), ALUG_L, ALUG_U, precond_4_solver,         &
     &           sigma_diag)
!
!C
!C-- ICCG computation

        ierr = 1
 
        write(*,*) 'init_solve33_DJDS_kemo', method_4_solver
      call init_solve33_DJDS_kemo(node%internal_node, node%numnod,      &
     &     NLmax, NUmax, itotal_l, itotal_u, NHYP, np_smp,              &
     &     node%istack_internal_smp, STACKmc, NLmaxHYP, NUmaxHYP,       &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u),  ALUG_L, ALUG_U, eps, itr, ierr,   &
     &     nod_comm%num_neib, nod_comm%id_neib,                         &
     &     nod_comm%istack_import, nod_comm%item_import,                &
     &     nod_comm%istack_export, NOD_EXPORT_NEW,                      &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn(node%numnod)

!
      end  subroutine solve_by_djds_solver33
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solverNN(node, tbl_crs, ierr)
!
      use calypso_mpi
      use m_machine_parameter
      use m_nod_comm_table
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
!
      use t_geometry_data
      use t_crs_matrix
!
      use m_solver_SR
      use solverNN_DJDS
      use preconditioning_DJDSNN

      use transfer_crs_2_djds
      use copy_matrix_2_djds_array
!
      type(node_data), intent(inout) :: node
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      integer(kind = kint), intent(inout) :: ierr
!
!
      call transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs)
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()

        write(*,*) 'resize_work_4_SR'
        if (nod_comm%num_neib .gt. 0) then
          call resize_work_4_SR(NB_djds, nod_comm%num_neib,             &
     &        nod_comm%istack_export(nod_comm%num_neib),                &
     &        nod_comm%istack_import(nod_comm%num_neib) )
        end if

        write(*,*) 'precond_DJDSNN'
        call precond_DJDSNN                                             &
     &    (node%internal_node, node%numnod, NB_djds, np_smp,            &
     &     node%istack_internal_smp, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,  &
     &     aiccg(im_d), ALUG_L, ALUG_U, precond_4_solver, sigma_diag)
!
!C
!C-- ICCG computation

        ierr = 1
        write(*,*) 'init_solveNN_DJDS_kemo', my_rank
!       if (my_rank.eq.0) write(*,*) 'init_solveNN_DJDS_kemo'
      call init_solveNN_DJDS_kemo                                       &
     &   ( node%internal_node, node%numnod, NB_djds, NLmax, NUmax,      &
     &     itotal_l, itotal_u, NHYP, np_smp, node%istack_internal_smp,  &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                  &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u), ALUG_L, ALUG_U, eps, itr, ierr,    &
     &     nod_comm%num_neib, nod_comm%id_neib,                         &
     &     nod_comm%istack_import, nod_comm%item_import,                &
     &     nod_comm%istack_export, NOD_EXPORT_NEW,                      &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn(node%numnod)

!
      end  subroutine solve_by_djds_solverNN
!
!  ---------------------------------------------------------------------
!
      end module solve_precond_DJDS

!
!      module DJDS_precond_solveNN
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve by DJDS solver using CRS matrix
!     results are also copied to CRS array
!
      module DJDS_precond_solveNN
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solverNN(ierr)
!
      use calypso_mpi
      use m_nod_comm_table
      use m_geometry_parameter
      use m_machine_parameter
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
!
      use m_solver_SR
      use solverNN_DJDS
      use preconditioning_DJDSNN

      use transfer_crs_2_djds
      use copy_matrix_2_djds_array
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      call transfer_crs_2_djds_matrix
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER  (CALYPSO_COMM, ierr_MPI)
        STARTTIME= MPI_WTIME()

        write(*,*) 'resize_work_4_SR'
        if (num_neib .gt. 0) then
          call resize_work_4_SR(NB_djds, num_neib,                      &
     &        istack_export(num_neib), istack_import(num_neib) )
        end if

        write(*,*) 'precond_DJDSNN'
        call precond_DJDSNN                                             &
     &         ( internal_node, numnod, NB_djds, np_smp,                &
     &           inter_smp_stack, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,     &
     &           aiccg(im_d), ALUG_L, ALUG_U, precond_4_solver,         &
     &           sigma_diag)
!
!C
!C-- ICCG computation

        ierr = 1
        write(*,*) 'init_solveNN_DJDS_kemo', my_rank
!       if (my_rank.eq.0) write(*,*) 'init_solveNN_DJDS_kemo'
      call init_solveNN_DJDS_kemo                                       &
     &   ( internal_node, numnod, NB_djds, NLmax, NUmax,                &
     &     itotal_l, itotal_u, NHYP, np_smp, inter_smp_stack,           &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT,                          &
     &     NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                  &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u),                                    &
     &     ALUG_L, ALUG_U, eps, itr, ierr, num_neib, id_neib,           &
     &     istack_import, item_import,                                  &
     &     istack_export, NOD_EXPORT_NEW,                               &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn

!
      end  subroutine solve_by_djds_solverNN
!
!  ---------------------------------------------------------------------
!
      end module DJDS_precond_solveNN

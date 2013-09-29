!
!      module DJDS_precond_solve11
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve by DJDS solver using CRS matrix
!     results are also copied to CRS array
!
!     subroutine solve_by_djds_solver11
!
      module DJDS_precond_solve11
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_by_djds_solver11
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_nod_comm_table
      use m_geometry_parameter
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
!
      call transfer_crs_2_djds_matrix
!C
!C== PRECONDITIONING
!
        call MPI_BARRIER(CALYPSO_COMM, ierr)
        STARTTIME= MPI_WTIME()
 
        ierr = 1
        if (num_neib .gt. 0) then
          call resize_work_4_SR(ione, num_neib,                        &
     &        istack_export(num_neib), istack_import(num_neib) )
        end if

        call precond_DJDS11                                             &
     &         ( internal_node, numnod, NLmax, itotal_l, NHYP, np_smp,  &
     &           inter_smp_stack, STACKmc, NLmaxHYP, IVECT,             &
     &           OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, LtoU,                &
     &           aiccg(im_d), indexDJDS_L, itemDJDS_L, aiccg(im_l),     &
     &           ALUG_L, ALUG_U, precond_4_solver, sigma_diag)
!
!C
!C-- ICCG computation

        write(*,*) 'init_solve_DJDS_kemo', my_rank
      call init_solve_DJDS_kemo                                         &
     &   ( internal_node, numnod, NLmax, NUmax, itotal_l, itotal_u,     &
     &     NHYP, np_smp, inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP,  &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u),                                    &
     &     ALUG_L, ALUG_U, eps, itr, ierr,                              &
     &     my_rank, num_neib, id_neib,                                  &
     &     istack_import, item_import,                                  &
     &     istack_export, NOD_EXPORT_NEW,                               &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn

!
      end  subroutine solve_by_djds_solver11
!
!  ---------------------------------------------------------------------
!
      end module DJDS_precond_solve11

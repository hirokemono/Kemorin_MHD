!
!      module solve_by_mass_z
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
!
!      subroutine solve_crs_by_mass_z
!      subroutine solve_crs_by_mass_z2
!
!      subroutine copy_solution_2_deltaz
!      subroutine copy_solution_2_delta_dz
!      subroutine copy_solution_2_d2dz
!
      module solve_by_mass_z
!
      use m_geometry_parameter
      use m_int_edge_vart_width
      use m_consist_mass_crs
!
      use m_precision
!
      implicit none
!
      real(kind = kreal) :: rtime, starttime, endtime
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z
!
      use m_parallel_var_dof
      use m_crs_connect
      use m_crs_matrix
      use DJDS_precond_solve11
!
      integer(kind = kint) :: i
!
!
      NB_crs = 1
         write(*,*) 'allocate_crs_mat_data'
      call allocate_crs_mat_data
!
      do i = 1, numnod
        D_crs(1,1,i) = d_mk_crs(i)
        B_crs(i) = rhs_mk_crs(i)
      end do
      do i = 1, ntot_crs_l
        AL_crs(1,1,i) = al_mk_crs(i)
      end do
      do i = 1, ntot_crs_u
        AU_crs(1,1,i) = au_mk_crs(i)
      end do
!
         write(*,*) 'solve_by_djds_solver11'
      call solve_by_djds_solver11
!
      do i = 1, numnod
        sol_mk_crs(i) = X_crs(i)
      end do
!
         write(*,*) 'deallocate_crs_mat_data'
      call deallocate_crs_mat_data
!
      end subroutine solve_crs_by_mass_z
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z2
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_nod_comm_table
      use m_geometry_parameter
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
      use m_crs_connect
      use m_crs_matrix
!
      use copy_matrix_2_djds_array
      use solver_DJDS
!
      integer(kind = kint) :: i
!
!
      NB_crs = 1
         write(*,*) 'allocate_crs_mat_data'
      call allocate_crs_mat_data
!
      do i = 1, numnod
        B_crs(i) = rhs_mk_crs(i)
      end do
!
      call copy_RH_vect_2_crs_nn
!
        write(*,*) 'init_solve_DJDS_kemo'
      call init_solve_DJDS_kemo                                         &
     &   ( internal_node, numnod, NLmax, NUmax, itotal_l, itotal_u,     &
     &     NHYP, np_smp, inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP,  &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u),                                    &
     &     ALUG_L, ALUG_U, eps, itr, ierr, num_neib, id_neib,           &
     &     istack_import, item_import,                                  &
     &     istack_export, NOD_EXPORT_NEW,                               &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn
!
      do i = 1, numnod
        sol_mk_crs(i) = X_crs(i)
      end do
!
         write(*,*) 'deallocate_crs_mat_data'
      call deallocate_crs_mat_data
!
      end subroutine solve_crs_by_mass_z2
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_deltaz
!
      integer(kind = kint) :: i
!
         do i = 1, numnod
          delta_z(i) = sol_mk_crs(i)
         end do
!
      end subroutine copy_solution_2_deltaz
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_delta_dz
!
      integer(kind = kint) :: i
!
         do i = 1, numnod
          delta_dz(i) = sol_mk_crs(i)
         end do
!
      end subroutine copy_solution_2_delta_dz
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_d2dz
!
      integer(kind = kint) :: i
!
         do i = 1, numnod
          d2_dz(i) = sol_mk_crs(i)
         end do
!
      end subroutine copy_solution_2_d2dz
!
!  ---------------------------------------------------------------------
!
      end module solve_by_mass_z
